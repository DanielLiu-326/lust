use std::alloc::{AllocError, Allocator};
use std::cell::Cell;
use std::marker::Unsize;
use std::ptr::NonNull;

use dst_init::EmplaceInitializer;
use crate::gc::{Gc, GcAlloc, GcObject};
use crate::state::GcState;
use crate::thin::Thin;
use crate::Phase::Sleep;
use crate::{Collectable, GcConfig, Phase};
use crate::util::Invariant;

//何时运行垃圾收集器？
//1.分配内存失败，执行完整收集过程
//2.内存系数上涨到一定阈值，内存阈值=f(x:上次内存大小，y:当前内存大小)
//
// 每次mutate后执行多少次step？
// 负债增加：arena做法：增加的内存大小 ，        我的做法：增加需要遍历的个数(alloc + added = grey size added) * factor
// 负债降低：arena做法：遍历的非greyagain大小，  我的做法：遍历过的的对象个数(1,0)

pub struct MutateHandle<'gc, 'state, A>
where
    A: Allocator,
{
    driver: &'state GcDriver<A>,
    _invar: Invariant<'gc>,
}

impl<'gc, 'state, A: Allocator> Clone for MutateHandle<'gc, 'state, A> {
    #[inline(always)]
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, 'state, A: Allocator> Copy for MutateHandle<'gc, 'state, A> {}

impl<'gc, 'state, A> MutateHandle<'gc, 'state, A>
where
    A: Allocator,
{
    #[inline(always)]
    pub fn create<T: Collectable>(&self, val: T) -> Gc<'gc, T> {
        self.driver.create(val)
    }

    #[inline(always)]
    pub fn emplace<Init: EmplaceInitializer>(&self, init: Init) -> Gc<'gc, Init::Output>
    where
        <Init as EmplaceInitializer>::Output: Collectable,
    {
        self.driver.emplace(init)
    }

    /// tell the garbage collector the child set of the parent has new child(s) during propagete;
    #[inline(always)]
    pub fn retrace<T: Collectable>(&self, parent: Gc<'gc, T>) {
        self.driver.retrace(parent)
    }

    #[inline(always)]
    pub fn root_retrace(&self) {
        self.driver.root_retrace()
    }

    #[inline(always)]
    pub fn full_collection<R: Collectable>(&self, root: &R) {
        self.driver.full_collection(root)
    }

    #[inline(always)]
    pub fn wakeup(&self) {
        self.driver.wakeup();
    }
}

pub struct GcDriver<A>
where
    A: Allocator,
{
    alloc: GcAlloc<A>,
    state: GcState,

    config: GcConfig,

    debt: Cell<isize>, // allocated
}

impl<A> GcDriver<A>
where
    A: Allocator,
{
    #[inline(always)]
    pub fn new(alloc: A, config: GcConfig) -> Self {
        let state = GcState::new();
        let alloc = GcAlloc::new(alloc);
        Self {
            alloc,
            state,
            config,
            debt: Cell::new(0),
        }
    }

    #[inline(always)]
    pub(crate) fn create<'gc, T: Collectable>(&self, mut val: T) -> Gc<'gc, T> {
        let obj = loop {
            match self.alloc.alloc(val) {
                Ok(o) => unsafe {
                    break o;
                },
                Err((e, t)) => {
                    val = t
                    //TODO FULL GC
                }
            }
        };
        let ret = self.state.create(obj);
        self.debt.set(self.debt.get() + ret.size() as isize);
        ret
    }

    #[inline(always)]
    pub(crate) fn emplace<'gc, Init: EmplaceInitializer>(
        &self,
        mut init: Init,
    ) -> Gc<'gc, Init::Output>
    where
        Init::Output: Collectable,
    {
        let obj = loop {
            match self.alloc.emplace(init) {
                Ok(o) => {
                    break o;
                }
                Err((e, t)) => {
                    init = t
                    //TODO FULL GC
                }
            }
        };
        let ret = self.state.create(obj);
        self.debt.set(self.debt.get() + ret.size() as isize);
        ret
    }

    #[inline(always)]
    pub(crate) fn retrace<T: Collectable>(&self, parent: Gc<'_, T>) {
        self.state.trace_need(parent)
    }

    #[inline(always)]
    pub(crate) fn root_retrace(&self) {
        self.state.root_added()
    }

    #[inline(always)]
    pub(crate) fn full_collection<R: Collectable>(&self, root: &R) {
        self.wakeup();
        loop {
            self.state.step(root, &self.alloc);
            if self.state.phase() == Sleep {
                break;
            }
        }
    }

    #[inline(always)]
    pub(crate) fn wakeup(&self) {
        self.state.wakeup()
    }

    #[inline(always)]
    pub(crate) fn wakeup_auto(&self) {
        if self.debt.get() > 0 {
            self.wakeup();
            self.debt
                .set((self.debt.get() * self.config.alloc_factor as isize) / 100);
        }
    }

    #[inline(always)]
    pub(crate) fn mutate_handle<'gc, 'state>(&'state self) -> MutateHandle<'gc, 'state, A> {
        MutateHandle {
            driver: &self,
            _invar: Default::default(),
        }
    }

    #[inline(always)]
    pub fn collect_auto<R: Collectable>(&self, root: &R) {
        if self.state.phase() == Sleep {
            self.wakeup_auto();
        } else {
            while self.debt.get() > 0 {
                let workload = self.step_counted(root);
                self.debt.set(self.debt.get() - workload as isize);
                if self.state.phase() == Sleep {
                    break;
                }
            }
        }
    }

    /// a function that will return how many workload a step done
    #[inline(always)]
    fn step_counted<R: Collectable>(&self, root: &R) -> usize {
        match self.state.phase() {
            Phase::Propagate => {
                let before = self.state.survived_size();
                self.state.step(root, &self.alloc);
                self.state.survived_size() - before
            }
            Phase::Sweep => {
                let before = self.state.total_size();
                self.state.step(root, &self.alloc);
                if self.state.phase() == Sleep {
                    self.finalize_collection();
                }
                before - self.state.total_size()
            }
            Sleep => 0,
        }
    }

    #[inline(always)]
    fn finalize_collection(&self) {
        // calculate debt for next start
        self.debt.set(-(self.state.total_size() as isize));
    }

    #[inline(always)]
    fn alloc_debt(&self, size: usize) {
        // update debt
        self.debt
            .set(self.debt.get() + (size as isize * self.config.alloc_factor as isize) / 100);
    }
}
