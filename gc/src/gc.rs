use crate::util::{unchecked_transmute, Invariant};
use crate::{Collectable, TraceHandle};
use dst_init::macros::dst;
use dst_init::{DirectInitializer, EmplaceInitializer};
use std::alloc::{AllocError, Allocator, Layout};
use std::cell::Cell;
use std::marker::Unsize;
use std::mem::transmute;
use std::ops::{CoerceUnsized, Deref};
use std::ptr::{self, drop_in_place, NonNull};

/// ObjectColor is the object status for object.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ObjectColor {
    White,
    Grey,
    Black,
}

/// The Gc Object Raw Memory Layout.
#[dst]
pub struct GcObject<T: ?Sized> {
    layout: Layout, // memory layout for whole GcObject
    vtable: Cell<CollectableVTable>,
    color: Cell<ObjectColor>,
    next: Cell<Option<GcGeneral>>,
    data: T,
}

impl<T: ?Sized> GcObject<T> {
    #[inline(always)]
    fn initializer<DstInit: EmplaceInitializer<Output = T>>(
        init: DstInit,
    ) -> GcObjectInit<T, DstInit> {
        let vtable = CollectableVTable::default();
        GcObjectInit {
            layout: Layout::from_size_align(0, 1).unwrap(),
            vtable: Cell::new(vtable),
            color: Cell::new(ObjectColor::White),
            next: Cell::new(None),
            data: init,
        }
    }
}

#[derive(Copy, Clone)]
pub struct CollectableVTable {
    meta: usize,
    trace: fn(NonNull<GcObject<General>>, usize, TraceHandle<'_>),
    drop: fn(NonNull<GcObject<General>>, usize),
}

impl Default for CollectableVTable {
    fn default() -> Self {
        Self {
            meta: 0,
            trace: |_, _, _| {},
            drop: |_, _| {},
        }
    }
}

impl CollectableVTable {
    #[inline(always)]
    fn new(
        meta: usize,
        trace: fn(NonNull<GcObject<General>>, usize, TraceHandle<'_>),
        drop: fn(NonNull<GcObject<General>>, usize),
    ) -> Self {
        Self { meta, trace, drop }
    }

    #[inline(always)]
    fn with_type<T: ?Sized + Collectable>(t: &T) -> CollectableVTable {
        unsafe {
            let meta = ptr::metadata(t as *const T);

            let trace: fn(NonNull<GcObject<General>>, usize, TraceHandle<'_>);
            if T::need_trace() {
                trace = |obj, meta, hdl| {
                    let t: NonNull<GcObject<T>> =
                        NonNull::from_raw_parts(obj.cast(), unchecked_transmute(meta));
                    t.as_ref().data.trace(hdl)
                };
            } else {
                trace = |_, _, _| {};
            }

            let drop = |obj: NonNull<GcObject<General>>, meta| {
                let mut t: NonNull<GcObject<T>> =
                    NonNull::from_raw_parts(obj.cast(), unchecked_transmute(meta));
                drop_in_place(&mut t.as_mut().data as _)
            };

            CollectableVTable::new(unchecked_transmute(meta), trace, drop)
        }
    }

    #[inline(always)]
    fn do_trace(self, obj: NonNull<GcObject<General>>, trace: TraceHandle<'_>) {
        (self.trace)(obj, self.meta, trace)
    }
}

/// Gc is a ptr to gc object.
#[rustc_nonnull_optimization_guaranteed]
pub struct Gc<'gc, T: 'gc + ?Sized> {
    obj: NonNull<GcObject<T>>,
    _invar: Invariant<'gc>,
}

impl<'gc, T: ?Sized> PartialEq for Gc<'gc, T> {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}

impl<'gc, T: ?Sized> Eq for Gc<'gc, T> {}

impl<T: ?Sized> Clone for Gc<'_, T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Gc<'_, T> {}

impl<T: ?Sized> Deref for Gc<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &self.obj.as_ref().data }
    }
}

impl<T: Unsize<U> + ?Sized, U: ?Sized> CoerceUnsized<Gc<'_, U>> for Gc<'_, T> {}

impl<'gc, T: ?Sized + 'gc> Gc<'gc, T> {
    #[inline(always)]
    pub fn obj(&self) -> &GcObject<T> {
        unsafe { self.obj.as_ref() }
    }

    #[inline(always)]
    pub fn color(&self) -> ObjectColor {
        self.obj().color.get()
    }

    #[inline(always)]
    pub fn set_color(&self, color: ObjectColor) {
        self.obj().color.set(color)
    }

    #[inline(always)]
    pub fn vtable(&self) -> CollectableVTable {
        self.obj().vtable.get()
    }

    #[inline(always)]
    pub fn set_vtable(&self, vtable: CollectableVTable) {
        self.obj().vtable.set(vtable)
    }

    #[inline(always)]
    pub fn next(&self) -> Option<GcGeneral> {
        self.obj().next.get()
    }

    #[inline(always)]
    pub fn set_next(&self, next: Option<GcGeneral>) {
        self.obj().next.set(next)
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.obj().layout.size()
    }

    #[inline(always)]
    pub unsafe fn as_mut(&mut self) -> &mut T {
        &mut self.obj.as_mut().data
    }

    #[allow(unused)]
    #[inline(always)]
    pub(crate) unsafe fn as_ptr(self) -> *mut GcObject<T> {
        self.obj.as_ptr()
    }

    #[inline(always)]
    pub(crate) unsafe fn inner(self) -> NonNull<GcObject<T>> {
        self.obj
    }
}

impl<T: ?Sized> Gc<'_, T> {
    #[inline(always)]
    pub(crate) fn create(obj: NonNull<GcObject<T>>) -> Self {
        Self {
            obj,
            _invar: Default::default(),
        }
    }

    #[inline(always)]
    pub(crate) unsafe fn upcast(self) -> GcGeneral {
        let Self { obj, .. } = self;
        GcGeneral {
            obj: transmute(obj.cast::<GcObject<()>>()),
            _invar: Default::default(),
        }
    }
}

pub struct General;

impl Collectable for General {
    fn trace(&self, _hdl: TraceHandle<'_>) {
        panic!("calling Collectable::trace on a general gc pointer")
    }

    fn need_trace() -> bool {
        true
    }
}

pub type GcGeneral = Gc<'static, General>;

impl GcGeneral {
    #[inline(always)]
    pub(crate) fn trace(&self, hdl: TraceHandle) {
        self.vtable().do_trace(self.obj, hdl)
    }
}

/// Gc allocator provides memory allocate function for gc_old.
pub(crate) struct GcAlloc<Alloc: Allocator> {
    alloc: Alloc,
}

impl<Alloc: Allocator> GcAlloc<Alloc> {
    #[inline(always)]
    pub const fn new(alloc: Alloc) -> Self {
        Self { alloc }
    }

    #[inline(always)]
    pub fn inner(&self) -> &Alloc {
        &self.alloc
    }

    #[inline(always)]
    pub fn emplace<Init: EmplaceInitializer>(
        &self,
        init: Init,
    ) -> Result<NonNull<GcObject<Init::Output>>, (AllocError, Init)>
    where
        <Init as EmplaceInitializer>::Output: Collectable,
    {
        let mut init = GcObject::initializer(init);
        let layout = init.layout();

        let mem = match self.alloc.allocate(layout) {
            Ok(mem) => mem,
            Err(e) => {
                let GcObjectInit { data, .. } = init;
                return Err((e, data));
            }
        };

        let mut ret = init.emplace(mem.cast());
        unsafe {
            ret.as_ref()
                .vtable
                .set(CollectableVTable::with_type(&ret.as_ref().data));
            ret.as_mut().layout = layout;
        }
        Ok(ret)
    }

    #[inline(always)]
    pub fn alloc<'gc, T: Collectable>(
        &self,
        t: T,
    ) -> Result<NonNull<GcObject<T>>, (AllocError, T)> {
        let init = DirectInitializer::new(t);
        self.emplace(init).map_err(|e| (e.0, e.1.fallback()))
    }

    #[inline(always)]
    pub fn dealloc<T: Collectable + ?Sized>(&self, t: NonNull<GcObject<T>>) {
        unsafe {
            (t.as_ref().vtable.get().drop)(t.cast(), unchecked_transmute(t.to_raw_parts().1));
            self.alloc.deallocate(t.cast(), t.as_ref().layout);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::gc::gc::{Gc, GcAlloc};
    use crate::gc::thin::{Thin, ThinInitializer};
    use crate::gc::{Collectable, GcAlloc, TraceHandle};
    use crate::thin::ThinInitializer;
    use crate::{Gc, Test, Thin};
    use dst_init::CoercionInitializer;
    use std::alloc::{alloc, Global};
    use std::fmt::Debug;
    use std::ops::Deref;

    impl Collectable for usize {
        fn trace(&self, hdl: TraceHandle<'_>) {}
    }
    impl Collectable for isize {
        fn trace(&self, hdl: TraceHandle<'_>) {}
    }
    impl Collectable for u32 {
        fn trace(&self, hdl: TraceHandle<'_>) {}
    }
    impl Collectable for u8 {
        fn trace(&self, hdl: TraceHandle<'_>) {}
    }
    impl Collectable for u64 {
        fn trace(&self, hdl: TraceHandle<'_>) {}
    }
    #[test]
    fn test() {
        fn test_alloc<T: Collectable + Debug + PartialEq>(a: T, b: T) {
            let allocator = GcAlloc::new(Global::default());
            let gc = Gc::create(allocator.alloc(a).unwrap());
            assert_eq!(*gc, b);
            allocator.dealloc(unsafe { gc.inner() });
        }
        test_alloc(1usize, 1usize);
        test_alloc(1u8, 1u8);
        test_alloc(4564456u32, 4564456u32);
        test_alloc(4564456u64, 4564456u64);

        fn test_thin<T: Debug + Collectable>(a: T, b: T) {
            pub trait Test: Debug + Collectable {}
            impl<T: ?Sized + Debug + Collectable> Test for T {}

            let allocator = GcAlloc::new(Global::default());
            let gc: Gc<Thin<dyn Test>> = Gc::create(
                allocator
                    .emplace(ThinInitializer::from(CoercionInitializer::new(a)))
                    .map_err(|a| ())
                    .unwrap(),
            );
            assert_eq!(format!("{:?}", &**gc), format!("{:?}", b)[..]);
            allocator.dealloc(unsafe { gc.inner() });
        }

        test_thin(1usize, 1usize);
        test_thin(1u8, 1u8);
        test_thin(4564456u32, 4564456u32);
        test_thin(4564456u64, 4564456u64);
    }
}
