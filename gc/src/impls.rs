use crate::{Collectable, Gc, TraceHandle};

use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::hash::{BuildHasher, Hash};
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;

pub macro impl_collect_nothing($t:ty) {
    impl Collectable for $t {
        fn trace(&self, _hdl: TraceHandle<'_>) {}
        fn need_trace() -> bool {
            false
        }
    }
}

impl_collect_nothing!(bool);
impl_collect_nothing!(char);
impl_collect_nothing!(u8);
impl_collect_nothing!(u16);
impl_collect_nothing!(u32);
impl_collect_nothing!(u64);
impl_collect_nothing!(usize);
impl_collect_nothing!(i8);
impl_collect_nothing!(i16);
impl_collect_nothing!(i32);
impl_collect_nothing!(i64);
impl_collect_nothing!(isize);
impl_collect_nothing!(f32);
impl_collect_nothing!(f64);
impl_collect_nothing!(String);
impl_collect_nothing!(str);
impl_collect_nothing!(core::ffi::CStr);
impl_collect_nothing!(core::any::TypeId);
impl_collect_nothing!(std::path::Path);
impl_collect_nothing!(std::path::PathBuf);
impl_collect_nothing!(std::ffi::OsStr);
impl_collect_nothing!(std::ffi::OsString);

impl<T: ?Sized> Collectable for &T {
    #[inline]
    fn need_trace() -> bool {
        false
    }
}

impl<T: ?Sized + Collectable> Collectable for Box<T> {
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        (**self).trace(hdl)
    }
}

impl<T: Collectable> Collectable for [T] {
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for t in self.iter() {
            t.trace(hdl)
        }
    }

    #[inline]
    fn need_trace() -> bool {
        T::need_trace()
    }
}

impl<T: Collectable> Collectable for Option<T> {
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        if let Some(t) = self.as_ref() {
            t.trace(hdl)
        }
    }

    #[inline]
    fn need_trace() -> bool {
        T::need_trace()
    }
}

impl<T: Collectable, E: Collectable> Collectable for Result<T, E> {
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        match self {
            Ok(r) => r.trace(hdl),
            Err(e) => e.trace(hdl),
        }
    }

    #[inline]
    fn need_trace() -> bool {
        T::need_trace() || E::need_trace()
    }
}

impl<T: Collectable> Collectable for Vec<T> {
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for t in self {
            t.trace(hdl)
        }
    }

    #[inline]
    fn need_trace() -> bool {
        T::need_trace()
    }
}

impl<T: Collectable> Collectable for VecDeque<T> {
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for t in self {
            t.trace(hdl)
        }
    }

    #[inline]
    fn need_trace() -> bool {
        T::need_trace()
    }
}

impl<K, V, S> Collectable for HashMap<K, V, S>
where
    K: Eq + Hash + Collectable,
    V: Collectable,
    S: BuildHasher + 'static,
{
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for (k, v) in self {
            k.trace(hdl);
            v.trace(hdl);
        }
    }

    #[inline]
    fn need_trace() -> bool {
        K::need_trace() || V::need_trace()
    }
}

impl<T, S> Collectable for HashSet<T, S>
where
    T: Eq + Hash + Collectable,
    S: BuildHasher + 'static,
{
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for v in self {
            v.trace(hdl);
        }
    }

    #[inline]
    fn need_trace() -> bool {
        T::need_trace()
    }
}

impl<K, V> Collectable for BTreeMap<K, V>
where
    K: Eq + Ord + Collectable,
    V: Collectable,
{
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for (k, v) in self {
            k.trace(hdl);
            v.trace(hdl);
        }
    }

    #[inline]
    fn need_trace() -> bool {
        K::need_trace() || V::need_trace()
    }
}

impl<T> Collectable for BTreeSet<T>
where
    T: Eq + Ord + Collectable,
{
    #[inline]
    fn need_trace() -> bool {
        T::need_trace()
    }

    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for v in self {
            v.trace(hdl);
        }
    }
}

impl<T> Collectable for Rc<T>
where
    T: ?Sized + Collectable,
{
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        (**self).trace(hdl);
    }
}

impl<T> Collectable for Arc<T>
where
    T: ?Sized + Collectable,
{
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        (**self).trace(hdl);
    }
}

impl<T> Collectable for Cell<T>
where
    T: 'static,
{
    #[inline]
    fn need_trace() -> bool {
        false
    }
}

impl<T> Collectable for RefCell<T>
where
    T: 'static,
{
    #[inline]
    fn need_trace() -> bool {
        false
    }
}

impl<T: ?Sized> Collectable for PhantomData<T> {
    #[inline]
    fn need_trace() -> bool {
        false
    }
}

impl<T: Collectable, const N: usize> Collectable for [T; N] {
    #[inline]
    fn trace(&self, hdl: TraceHandle<'_>) {
        for t in self {
            t.trace(hdl)
        }
    }

    #[inline]
    fn need_trace() -> bool {
        T::need_trace()
    }
}

macro_rules! impl_tuple {
    () => (
        impl Collectable for () {
            #[inline]
            fn need_trace() -> bool {
                false
            }
        }
    );

    ($($name:ident)+) => (
        impl<$($name,)*> Collectable for ($($name,)*)
            where $($name: Collectable,)*
        {
            #[inline]
            fn need_trace() -> bool {
                $($name::need_trace() ||)* false
            }

            #[allow(non_snake_case)]
            #[inline]
            fn trace(&self, hdl: TraceHandle<'_>) {
                let ($($name,)*) = self;
                $($name.trace(hdl);)*
            }
        }
    );
}

impl_tuple! {}
impl_tuple! {A}
impl_tuple! {A B}
impl_tuple! {A B C}
impl_tuple! {A B C D}
impl_tuple! {A B C D E}
impl_tuple! {A B C D E F}
impl_tuple! {A B C D E F G}
impl_tuple! {A B C D E F G H}
impl_tuple! {A B C D E F G H I}
impl_tuple! {A B C D E F G H I J}
impl_tuple! {A B C D E F G H I J K}
impl_tuple! {A B C D E F G H I J K L}
impl_tuple! {A B C D E F G H I J K L M}
impl_tuple! {A B C D E F G H I J K L M N}
impl_tuple! {A B C D E F G H I J K L M N O}
impl_tuple! {A B C D E F G H I J K L M N O P}

pub trait CollectableField {
    fn trace_or_reached(&self, hdl: TraceHandle<'_>);
}
impl<'gc, T: ?Sized> CollectableField for Gc<'gc, T> {
    fn trace_or_reached(&self, mut hdl: TraceHandle<'_>) {
        hdl.reached(self.clone())
    }
}

impl<T: Collectable + ?Sized> CollectableField for T {
    fn trace_or_reached(&self, hdl: TraceHandle<'_>) {
        if T::need_trace() {
            self.trace(hdl)
        }
    }
}
