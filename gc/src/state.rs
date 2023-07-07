use crate::gc::ObjectColor::{Black, Grey, White};
use crate::gc::{CollectableVTable, Gc, GcAlloc, GcGeneral, GcObject};
use crate::thin::Thin;
use crate::Collectable;
use crate::Phase::{Propagate, Sleep, Sweep};
use std::alloc::{AllocError, Allocator};
use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::marker::Unsize;
use std::ptr::NonNull;

/// Garbage Collector Phase
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Phase {
    Propagate,
    Sweep,
    Sleep,
}

#[derive(Copy, Clone)]
pub struct TraceHandle<'state> {
    state: &'state GcState,
}

impl<'state> TraceHandle<'state> {
    #[inline(always)]
    pub(crate) fn new(state: &'state GcState) -> Self {
        Self { state }
    }
    /// tell garbage collector the object is reachable
    #[inline(always)]
    pub fn reached<T: ?Sized>(&mut self, gc_obj: Gc<T>) {
        self.state.reached(gc_obj);
    }
}

/// Garbage Collector State
pub struct GcState {
    phase: Cell<Phase>,

    alive_size: Cell<usize>,    // total size of allocated object.
    alive: Cell<usize>,         // now alive object number
    survived_size: Cell<usize>, // total size of touched objects

    root_need_trace: Cell<bool>,  //whether root needs trace
    all: Cell<Option<GcGeneral>>, // all created objects

    sweep: Cell<Option<GcGeneral>>,
    sweep_prev: Cell<Option<GcGeneral>>,

    grey: RefCell<VecDeque<GcGeneral>>,
    grey_again: RefCell<VecDeque<GcGeneral>>,
}

impl GcState {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            phase: Cell::new(Phase::Sleep),

            alive_size: Cell::new(0),
            alive: Cell::new(0),
            survived_size: Cell::new(0),

            root_need_trace: Cell::new(false),
            all: Cell::new(None),
            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            grey: RefCell::new(VecDeque::new()),
            grey_again: RefCell::new(Default::default()),
        }
    }

    #[inline(always)]
    pub fn trace_need<T: Collectable>(&self, obj: Gc<'_, T>) {
        if self.phase.get() == Propagate && obj.color() == Black {
            obj.set_color(Grey);
            self.grey_again
                .borrow_mut()
                .push_back(unsafe { obj.upcast() });
        }
    }

    #[inline(always)]
    pub fn reached<T: ?Sized>(&self, obj: Gc<'_, T>) {
        if obj.color() == White {
            obj.set_color(Grey);
            self.grey.borrow_mut().push_back(unsafe { obj.upcast() });
        }
    }

    #[inline(always)]
    pub fn root_added(&self) {
        if self.phase.get() == Propagate {
            self.root_need_trace.set(true);
        }
    }

    #[inline(always)]
    pub fn wakeup(&self) {
        if self.phase.get() == Sleep {
            self.root_need_trace.set(true);
            self.phase.set(Propagate);
            self.survived_size.set(0);
        }
    }

    /// propagate one step, close when
    #[inline(always)]
    pub fn propagate<T: Collectable>(&self, root: &T) {
        let grey = if let Some(grey) = self.grey.borrow_mut().pop_front() {
            self.survived_size
                .set(self.survived_size.get() + grey.size());
            Some(grey)
        } else if let Some(grey) = self.grey_again.borrow_mut().pop_front() {
            Some(grey)
        } else {
            None
        };

        if let Some(grey) = grey {
            grey.set_color(Black);
            grey.trace(TraceHandle::new(self));
        } else if self.root_need_trace.get() {
            self.root_need_trace.set(false);
            root.trace(TraceHandle::new(self))
        } else {
            self.sweep.set(self.all.get());
            self.sweep_prev.set(None);
            self.phase.set(Phase::Sweep); // enter sweep phase
        }
    }

    /// sweep phase. sweep objects and mark as white for next gc cycle.
    #[inline(always)]
    pub(crate) fn sweep<T: Collectable, Alloc: Allocator>(&self, root: &T, alloc: &GcAlloc<Alloc>) {
        // 1. clean the white object
        // 2. make all iterated object turn white.
        let Some(sweep) = self.sweep.get() else {
            // sweep done, enter sleep
            self.phase.set(Sleep);
            return;
        };

        if sweep.color() == White {
            self.alive.set(self.alive.get() - 1);
            self.alive_size.set(self.alive_size.get() - sweep.size());

            // remove object from list
            if let Some(prev) = self.sweep_prev.get() {
                prev.set_next(sweep.next());
            }
            self.sweep.set(sweep.next());

            unsafe {
                if let Some(all) = self.all.get() {
                    if all.inner() == sweep.inner() {
                        self.all.set(None);
                    }
                }
                // delete object
                dbg!("gc swept!");
                alloc.dealloc(sweep.inner());
            }
        } else {
            sweep.set_color(White); //set color white for next collection
            self.sweep_prev.set(Some(sweep));
            self.sweep.set(sweep.next());
        }
    }

    #[inline(always)]
    pub(crate) fn step<R, Alloc>(&self, root: &R, alloc: &GcAlloc<Alloc>)
    where
        R: Collectable,
        Alloc: Allocator,
    {
        match self.phase.get() {
            Propagate => self.propagate(root),
            Sweep => self.sweep(root, alloc),
            Sleep => {}
        }
    }

    #[inline(always)]
    pub fn phase(&self) -> Phase {
        self.phase.get()
    }

    /// the number of grey objects
    #[inline(always)]
    pub fn grey(&self) -> usize {
        self.grey.borrow_mut().len()
    }

    #[inline(always)]
    pub fn total_size(&self) -> usize {
        self.alive_size.get()
    }

    /// create an object
    #[inline(always)]
    pub(crate) fn create<'gc, T>(&self, obj: NonNull<GcObject<T>>) -> Gc<'gc, T>
    where
        T: Collectable + ?Sized,
    {
        let obj = Gc::create(obj);
        // insert object to list
        obj.set_next(self.all.get());
        self.all.set(Some(unsafe { obj.upcast() }));

        // tell the gc to trace the object
        if self.phase.get() == Propagate {
            obj.set_color(Grey);
            self.grey.borrow_mut().push_back(unsafe { obj.upcast() });
        } else if self.phase.get() == Sweep {
            // sweep, White
            if self.sweep_prev.get().is_none() {
                self.sweep_prev.set(Some(unsafe { obj.upcast() }));
            }
            obj.set_color(White);
        } else {
            // sleep, white
            obj.set_color(White);
        }

        // update statistic info
        self.alive.set(self.alive.get() + 1);
        self.alive_size.set(self.alive_size.get() + obj.size());

        obj
    }

    #[inline(always)]
    pub fn survived_size(&self) -> usize {
        self.survived_size.get()
    }
}
