use gc::{Collectable, TraceHandle};
use std::fmt::{Debug, Formatter};
use std::slice::SliceIndex;

use crate::value::{Nil, Value};
use crate::vm::opcode::Register;
use crate::vm::StackFrame;
//two stack,1 for register,1 for call stack
// register redirect : 0: this pointer

pub struct VmStack<'gc, F> {
    registers: [Value<'gc>; 1000],
    frames: [Option<F>; 128],
    bs_stack: [(usize, usize); 128],
    top: usize,
    bs: usize,
}

impl<'gc, F> VmStack<'gc, F> {
    pub(crate) fn new(size: usize, f: F) -> Self {
        let registers = [(); 1000].map(|_| Value::Nil(()));
        let mut frames = [(); 128].map(|_| None);
        frames[0] = Some(f);
        Self {
            registers,
            frames,
            bs_stack: [(0, size); 128],
            top: 1,
            bs: 0,
        }
    }
}

impl<'gc, F> Default for VmStack<'gc, F> {
    fn default() -> Self {
        let registers = [(); 1000].map(|_| Value::Nil(()));
        let frames = [(); 128].map(|_| None);
        Self {
            registers,
            frames,
            bs_stack: [(0, 0); 128],
            top: 0,
            bs: 0,
        }
    }
}

impl<'gc, F: Collectable> Collectable for VmStack<'gc, F> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        for i in 0..self.top {
            self.frames[i].as_ref().unwrap().trace(hdl)
        }
        if self.top != 0 {
            for i in self.bs_stack[0].0..self.bs_stack[self.top].1 {
                self.registers[i].trace(hdl)
            }
        }
    }
}

impl<'gc, F> VmStack<'gc, F> {
    #[inline(always)]
    fn bs(&self) -> usize {
        unsafe { self.bs_stack.get_unchecked(self.top - 1).0 }
    }

    #[inline(always)]
    fn end(&self) -> usize {
        unsafe { self.bs_stack.get_unchecked(self.top).1 }
    }

    #[inline(always)]
    pub fn register(&self, reg: u8) -> Value<'gc> {
        unsafe { *self.registers.get_unchecked(self.bs + reg as usize) }
    }

    #[inline(always)]
    pub fn register_mut(&mut self, reg: u8) -> &mut Value<'gc> {
        unsafe { self.registers.get_unchecked_mut(self.bs + reg as usize) }
    }

    #[inline(always)]
    pub fn push_frame(&mut self, base: Register, size: usize, f: F) {
        unsafe {
            *self.frames.get_unchecked_mut(self.top) = Some(f);
            *self.bs_stack.get_unchecked_mut(self.top) =
                (self.bs() + base as usize, self.bs() + base as usize + size);
            self.top += 1;
            self.bs = self.bs();
        }
    }

    #[inline(always)]
    pub fn pop_frame(&mut self) -> Option<F> {
        unsafe {
            let top = self.frames.get_unchecked_mut(self.top - 1).take();
            self.top -= 1;
            self.bs = self.bs();
            top
        }
    }

    #[inline(always)]
    pub fn top(&self) -> Option<&F> {
        unsafe { self.frames.get_unchecked(self.top - 1).as_ref() }
    }
}

impl<'gc, F> Debug for VmStack<'gc, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        unsafe { todo!() }
    }
}
