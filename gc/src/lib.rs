#![feature(allocator_api)]
#![feature(ptr_metadata)]
#![feature(unsize)]
#![feature(layout_for_ptr)]
#![feature(decl_macro)]
#![feature(rustc_attrs)]
#![feature(negative_impls)]
#![feature(alloc_layout_extra)]
#![feature(const_mut_refs)]
#![feature(const_swap)]
#![feature(coerce_unsized)]

mod config;
mod driver;
mod gc;
mod state;
pub mod thin;
pub mod impls;
mod util;

use std::alloc::Allocator;

use driver::GcDriver;

//====== Exports ======
pub use config::GcConfig;
pub use driver::MutateHandle;
pub use gc::Gc;
pub use state::{Phase, TraceHandle};
pub use thin::Thin;
pub use gc_derive::Collectable;

pub trait Collectable {
    fn trace(&self, hdl: TraceHandle<'_>) {}
    fn need_trace() -> bool {
        true
    }
}

#[macro_export]
macro_rules! RootableTy {
    ($root:ty) => {
        // Instead of generating an impl of `Rootable`, we use a trait object. Thus, we avoid the
        // need to generate a new type for each invocation of this macro.
        dyn for<'gc> $crate::Rootable<'gc, R = $root>
    };
}

pub trait Rootable<'gc> {
    type R: Collectable + 'gc;
}

pub type Root<'a, R> = <R as Rootable<'a>>::R;

pub struct GarbageCollector<A, R>
    where
        A: Allocator,
        R: for<'a> Rootable<'a> + ?Sized,
{
    root: Root<'static, R>,
    driver: GcDriver<A>,
}

impl<A, R> GarbageCollector<A, R>
    where
        A: Allocator,
        R: for<'a> Rootable<'a> + ?Sized,
{
    #[inline(always)]
    pub fn new<F>(alloc: A, config: GcConfig, root_init: F) -> Self
        where
            F: for<'gc> FnOnce(MutateHandle<'gc, '_, A>) -> Root<'gc, R>,
    {
        let driver = GcDriver::new(alloc, config);
        let hdl = driver.mutate_handle();
        let root = root_init(hdl);
        unsafe {
            Self {
                root: std::mem::transmute(root),
                driver,
            }
        }
    }

    #[inline(always)]
    pub fn mutate<'a, F, Ret>(&'a mut self, f: F) -> Ret
        where
            F: for<'gc> FnOnce(&'a mut Root<'gc, R>, MutateHandle<'gc, 'a, A>) -> Ret,
    {
        f(&mut self.root, self.driver.mutate_handle())
    }

    #[inline(always)]
    pub fn full_collect(&self) {
        self.driver.full_collection(&self.root);
    }

    #[inline(always)]
    pub fn wakeup(&self) {
        self.driver.wakeup();
    }

    #[inline(always)]
    pub fn collect_auto(&self) {
        self.driver.collect_auto(&self.root);
    }
}

// ==== async design ====
// let gc = GarbageCollector::new(GcConfig::default(),|mc|{
//
// });
// gc.mutate(|root:&mut Root,hdl|{
//
// });
//
