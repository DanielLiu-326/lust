use std::any::{Any, TypeId};

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::marker::{PhantomData, Unsize};
use std::mem::MaybeUninit;
use std::ops::AddAssign;
use std::ptr::{null, Pointee};
use std::{mem, ptr};

pub type Invariant<'a> = PhantomData<Cell<&'a ()>>;

pub type Metadata<T> = <T as Pointee>::Metadata;

#[inline(always)]
pub const fn metadata_of<T: Unsize<Dyn>, Dyn: ?Sized>() -> Metadata<Dyn> {
    let null: *const T = null();
    let dyn_null = null as *const Dyn;
    ptr::metadata(dyn_null)
}

#[inline(always)]
pub const fn id_of<T: Any>() -> TypeId {
    TypeId::of::<T>()
}

#[inline(always)]
pub const unsafe fn unchecked_transmute<Dst, Src>(mut src: Src) -> Dst {
    unsafe {
        let mut ret = MaybeUninit::uninit();
        mem::swap::<Dst>(mem::transmute(&mut src), mem::transmute(&mut ret));
        mem::forget(src);
        ret.assume_init()
    }
}

#[inline(always)]
pub fn mem_cmp<T, const SIZE: usize>(src: &T, dst: &T) -> bool {
    unsafe {
        let a: *const [u8; SIZE] = (src as *const T).cast();
        let b: *const [u8; SIZE] = (dst as *const T).cast();
        return *a == *b;
    }
}

pub struct Cell<T: ?Sized> {
    value: T,
}
impl<T> Cell<T> {
    #[inline(always)]
    pub(crate) fn new(t: T) -> Self {
        Self { value: t }
    }
}

impl<T: Default> Default for Cell<T> {
    fn default() -> Self {
        Self {
            value: Default::default(),
        }
    }
}

impl<T> Cell<T> {
    pub fn replace(&self, t: T) -> T {
        unsafe { mem::replace(&mut *(&self.value as *const T as usize as *mut T), t) }
    }
}

impl<T: Copy> Cell<T> {
    #[inline(always)]
    pub fn get(&self) -> T {
        self.value
    }

    #[inline(always)]
    pub fn set(&self, t: T) {
        self.replace(t);
    }
}

unsafe impl<T: ?Sized> Send for Cell<T> where T: Send {}

impl<T: ?Sized> !Sync for Cell<T> {}

impl<T: Clone> Clone for Cell<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self::new(self.value.clone())
    }
}

impl<T: Copy> Copy for Cell<T> {}

impl<T: PartialEq + Copy> PartialEq for Cell<T> {
    #[inline]
    fn eq(&self, other: &Cell<T>) -> bool {
        self.get() == other.get()
    }
}

pub macro ok_likely($expr:expr) {
    unsafe {
        let val = $expr;
        if std::intrinsics::likely(val.is_ok()) {
            val.unwrap_unchecked()
        } else {
            return Err(val.unwrap_err_unchecked().into());
        }
    }
}

#[derive(Clone)]
pub struct MinimumIdAlloc<T> {
    max: T,
    alloc: T,
    recycle: BinaryHeap<Reverse<T>>,
}

impl<T> MinimumIdAlloc<T>
where
    T: From<u8> + Ord + AddAssign + Copy + std::ops::SubAssign,
{
    pub fn new(start: T, max: T) -> Self {
        Self {
            alloc: start,
            max,
            recycle: BinaryHeap::new(),
        }
    }

    pub fn allocate_top(&mut self) -> Option<T> {
        let top = self.top();
        if top == self.max {
            return None;
        }
        self.alloc += T::from(1u8);
        Some(top)
    }

    pub fn allocate(&mut self) -> Option<T> {
        if !self.recycle.is_empty() {
            return self.recycle.pop().map(|a| a.0);
        }

        if self.alloc == self.max {
            return None;
        }

        let alloc = self.alloc;
        self.alloc += T::from(1u8);
        return Some(alloc);
    }

    pub fn recycle(&mut self, id: T) {
        if id > self.alloc {
            panic!("id has not been allocated")
        }
        self.recycle.push(Reverse(id));
    }

    // return the id to be allocated at the next time
    pub fn top(&mut self) -> T {
        let mut recycle = self.recycle.clone().into_sorted_vec();
        recycle.reverse();
        while let Some(Reverse(mut id)) = recycle.last() {
            id += T::from(1u8);
            if self.alloc != id {
                break;
            } else {
                self.alloc -= T::from(1u8);
            }
            recycle.pop();
        }
        self.recycle = BinaryHeap::from(recycle);
        return self.alloc;
    }

    pub fn max(&self) -> T {
        self.alloc
    }
}
