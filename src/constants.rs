use gc::{Collectable, MutateHandle, TraceHandle};
use macros::mux;
use std::alloc::Allocator;

use crate::value::{Bool, Float, FnProto, Integer, Nil, OpError, String, Value};

/// const a = 100;
/// const func_a;
/// func_a = fn_test(){
///     let a = 100;
///     let b = 10000;
///     return a+b;
/// };

#[mux]
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Constant<'gc> {
    Integer(Integer),
    Float(Float),
    FnProto(FnProto<'gc>),
    String(String<'gc>),
}

impl<'gc> Collectable for Constant<'gc> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        constant_match!(self => this,{
            this.trace(hdl)
        })
    }
}

impl<'gc> Constant<'gc> {
    pub(crate) fn load<A: Allocator>(
        self,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        match self {
            Constant::Integer(val) => Ok(Value::Integer(val)),
            Constant::Float(val) => Ok(Value::Float(val)),
            Constant::FnProto(_) => Err(OpError::NotSupport),
            Constant::String(t) => Ok(Value::String(t)),
        }
    }
}
