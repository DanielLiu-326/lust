use gc::{Collectable, TraceHandle};

use crate::constants::Constant;
use crate::vm::opcode::ConstAddr;

pub struct ConstTbl<'gc> {
    consts: Vec<Constant<'gc>>,
}

impl<'gc> Collectable for ConstTbl<'gc> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        for x in &self.consts {
            x.trace(hdl);
        }
    }
}

impl<'gc> ConstTbl<'gc> {
    pub(crate) fn new(consts: Vec<Constant<'gc>>) -> Self {
        Self { consts }
    }

    pub(crate) fn get(&self, co: usize, addr: ConstAddr) -> Constant<'gc> {
        self.consts[co + addr as usize]
    }
}
