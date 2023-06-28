use std::any::{Any, TypeId};
use std::cell::{UnsafeCell};
use std::marker::{PhantomData, Unsize};
use std::mem::{size_of, MaybeUninit, transmute};
use std::ops::{AddAssign, Deref};
use std::ptr::{null, Pointee};
use std::{any, mem, ptr};
use std::collections::BinaryHeap;

pub type Invariant<'a> = PhantomData<Cell<&'a ()>>;

pub type Metadata<T> = <T as Pointee>::Metadata;

#[inline(always)]
pub const fn metadata_of<T: Unsize<Dyn>, Dyn: ?Sized>() -> Metadata<Dyn> {
    let null: *const T = null();
    let dyn_null = null as *const Dyn;
    ptr::metadata(dyn_null)
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

pub struct Cell<T:?Sized>{
    value:T,
}
impl<T> Cell<T>{
    #[inline(always)]
    pub(crate) fn new(t:T) ->Self{
        Self{
            value: t,
        }
    }
}

impl<T:Default> Default for Cell<T>{
    fn default() -> Self {
        Self{
            value: Default::default(),
        }
    }
}

impl<T> Cell<T>{
    pub fn replace(&self,t:T)->T{unsafe{
        mem::replace(&mut *(&self.value as *const T as usize as *mut T),t)
    }}
}

impl <T:Copy> Cell<T>{
    #[inline(always)]
    pub fn get(&self)->T{
        self.value
    }

    #[inline(always)]
    pub fn set(&self,t:T){unsafe{
        self.replace(t);
    }}
}

unsafe impl<T: ?Sized> Send for Cell<T> where T: Send {}

impl<T: ?Sized> !Sync for Cell<T> {}

impl<T:Clone> Clone for Cell<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self::new(self.value.clone())
    }
}

impl<T:Copy> Copy for Cell<T>{}

impl<T: PartialEq + Copy> PartialEq for Cell<T> {
    #[inline]
    fn eq(&self, other: &Cell<T>) -> bool {
        self.get() == other.get()
    }
}

pub macro ok_likely($expr:expr){{
    let val = $expr;
    if std::intrinsics::likely(val.is_ok()){
        unsafe{val.unwrap_unchecked()}
    }else{
        return Err(unsafe{val.unwrap_err_unchecked().into()})
    }
}}


#[derive(Clone)]
pub struct MinimumAlloc<T>{
    max:T,
    alloc:T,
    recycle:BinaryHeap<T>
}
