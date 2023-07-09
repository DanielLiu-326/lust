use crate::util::Metadata;
use crate::{Collectable, TraceHandle};
use dst_init::EmplaceInitializer;
use std::alloc::Layout;

use std::mem::{align_of_val_raw, size_of};
use std::ops::{Deref, DerefMut};
use std::ptr;
use std::ptr::NonNull;

#[repr(C)]
pub struct Thin<Dyn: ?Sized> {
    meta: Metadata<Dyn>,
    data: (),
}

pub struct ThinInitializer<Init: EmplaceInitializer> {
    data: Init,
}

impl<Init: EmplaceInitializer> From<Init> for ThinInitializer<Init> {
    fn from(value: Init) -> Self {
        Self { data: value }
    }
}

impl<Init: EmplaceInitializer> EmplaceInitializer for ThinInitializer<Init> {
    type Output = Thin<Init::Output>;

    #[inline(always)]
    fn layout(&mut self) -> Layout {
        Layout::new::<Metadata<Init::Output>>()
            .extend(self.data.layout())
            .unwrap()
            .0
            .pad_to_align()
    }

    #[inline(always)]
    fn emplace(mut self, ptr: NonNull<u8>) -> NonNull<Self::Output> {
        unsafe {
            let meta = ptr.as_ptr().cast::<Metadata<Init::Output>>();
            let align = self.data.layout().align();
            let padding = Layout::new::<Metadata<Init::Output>>().padding_needed_for(align);
            let data = ptr
                .as_ptr()
                .add(size_of::<Metadata<Init::Output>>())
                .add(padding);
            let data = self.data.emplace(NonNull::new_unchecked(data));
            meta.write(data.to_raw_parts().1);
            ptr.cast()
        }
    }
}

impl<Dyn: Collectable + ?Sized> Collectable for Thin<Dyn> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        self.deref().trace(hdl);
    }
}

impl<Dyn: ?Sized> Deref for Thin<Dyn> {
    type Target = Dyn;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe {
            let align = align_of_val_raw(ptr::from_raw_parts::<Dyn>(ptr::null(), self.meta));
            let padding = Layout::new::<Metadata<Dyn>>().padding_needed_for(align);
            let unalign = (&self.data as *const ()).cast::<u8>();
            let aligned = ptr::from_raw_parts(unalign.add(padding).cast(), self.meta);
            &*aligned
        }
    }
}

impl<Dyn: ?Sized> DerefMut for Thin<Dyn> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let align = align_of_val_raw(ptr::from_raw_parts::<Dyn>(ptr::null(), self.meta));
            let padding = Layout::new::<Metadata<Dyn>>().padding_needed_for(align);
            let unalign = (&mut self.data as *mut ()).cast::<u8>();
            let aligned = ptr::from_raw_parts_mut(unalign.add(padding).cast(), self.meta);
            &mut *aligned
        }
    }
}

impl<Dyn: ?Sized> Drop for Thin<Dyn> {
    fn drop(&mut self) {
        unsafe {
            let data = self.deref_mut() as *mut Dyn;
            ptr::drop_in_place(data)
        }
    }
}
