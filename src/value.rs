use dst_init::macros::dst;
use dst_init::{DirectInitializer, EmplaceInitializer, Slice, SliceExt};
use gc::thin::ThinInitializer;
use gc::{Collectable, GarbageCollector, Gc, MutateHandle, Thin, TraceHandle};
use macros::mux;
use std::alloc::Allocator;
use std::collections::HashMap;
use std::ffi::CString;
use std::fmt::{Debug, Display, Formatter, Pointer};
use std::ops::{Add, Deref};

use crate::util::{ok_likely, Cell};

// ==========================================Primitives==========================================

pub type Integer = i64;
pub type Float = f64;
pub type Bool = bool;
pub type Nil = ();

// ==========================================String==========================================

#[derive(Collectable)]
#[dst]
pub struct StringObj {
    hash: usize,
    data: [u8],
}

// ==========================================FnProto==========================================
#[derive(Copy, Clone, Default, Debug, Collectable)]
pub struct FnMeta {
    pub pc: usize,
    pub param_num: usize,
    pub stack_size: usize,
    pub constant_offset: usize,
}

#[derive(Debug, Collectable)]
#[dst]
pub struct FnProtoObj {
    meta: FnMeta,
    captures: [usize],
}

#[derive(Copy, Clone, PartialEq, Collectable)]
pub struct FnProto<'gc> {
    gc: Gc<'gc, FnProtoObj>,
}

impl Debug for FnProto<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.gc.deref().fmt(f)
    }
}

impl<'gc> FnProto<'gc> {
    #[inline(always)]
    pub fn alloc<A, E>(meta: FnMeta, captures: E, hdl: MutateHandle<'gc, '_, A>) -> Self
    where
        A: Allocator,
        E: EmplaceInitializer<Output = [usize]>,
    {
        let init = FnProtoObjInit { meta, captures };
        let obj = hdl.emplace(init);
        Self { gc: obj }
    }

    #[inline(always)]
    pub fn meta(&self) -> &FnMeta {
        &self.gc.meta
    }

    #[inline(always)]
    pub fn captures(&self) -> &[usize] {
        &self.gc.captures
    }
}

// ==========================================Vector==========================================
#[derive(Copy, Clone)]
pub struct Vector<'gc> {
    gc: Gc<'gc, Vec<Value<'gc>>>,
}

impl Debug for Vector<'_>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Vector{:?}",self.gc.deref())
    }
}

impl<'gc> Vector<'gc> {
    pub fn pop(&mut self, val: UpValue) -> Option<Value<'gc>> {
        unsafe { self.gc.as_mut().pop() }
    }
    pub fn set(&mut self, index: usize, val: Value<'gc>) {
        unsafe {
            self.gc.as_mut().resize_with(index + 1, || Value::Nil(()));
            *self.gc.as_mut().last_mut().unwrap_unchecked() = val;
        }
    }
    pub fn get(&self, index: usize) -> Value<'gc> {
        if let Some(a) = self.gc.get(index) {
            return *a;
        } else {
            return Value::Nil(());
        }
    }
    pub fn len(&self) -> Integer {
        self.gc.len() as Integer
    }
    pub fn push(&mut self, val: Value<'gc>) {
        unsafe {
            self.gc.as_mut().push(val);
        }
    }
}

impl<'gc> Collectable for Vector<'gc> {
    fn trace(&self, mut hdl: TraceHandle<'_>) {
        hdl.reached(self.gc)
    }
}

// ==========================================Closure==========================================

#[dst]
pub struct ClosureObj<'gc> {
    meta: FnMeta,
    up_values: [UpValue<'gc>],
}

impl<'gc> Collectable for ClosureObj<'gc> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        for x in self.up_values.iter() {
            x.trace(hdl);
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Closure<'gc> {
    gc: Gc<'gc, Thin<ClosureObj<'gc>>>,
}

impl Debug for Closure<'_>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Closure")
    }
}

impl<'gc> Collectable for Closure<'gc> {
    fn trace(&self, mut hdl: TraceHandle<'_>) {
        hdl.reached(self.gc)
    }
}

impl<'gc> Closure<'gc> {
    pub fn default<A: Allocator>(hdl: MutateHandle<'gc, '_, A>) -> Self {
        let gc = hdl.emplace(ThinInitializer::from(ClosureObjInit {
            meta: FnMeta::default(),
            up_values: Slice::iter_init(0, (0..).map(|a| unreachable!())),
        }));
        Self { gc }
    }

    pub fn new<A: Allocator, E: EmplaceInitializer<Output = [UpValue<'gc>]>>(
        proto: FnProto<'gc>,
        caps: E,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Self {
        let init = ThinInitializer::from(ClosureObjInit {
            meta: *proto.meta(),
            up_values: caps,
        });
        let obj = hdl.emplace(init);
        Self { gc: obj }
    }

    #[inline(always)]
    pub fn meta(&self) -> &FnMeta {
        &self.gc.meta
    }

    #[inline(always)]
    pub fn up_values(&self) -> &[UpValue<'gc>] {
        &self.gc.up_values
    }
}

// ==========================================UpValue==========================================

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct UpValue<'gc> {
    gc: Gc<'gc, Value<'gc>>,
}

impl Debug for UpValue<'_>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "UpValue{{{:?}}}", self.gc.deref())
    }
}

impl<'gc> Collectable for UpValue<'gc> {
    fn trace(&self, mut hdl: TraceHandle<'_>) {
        hdl.reached(self.gc)
    }
}

impl<'gc> UpValue<'gc> {
    #[inline(always)]
    pub fn new<A: Allocator>(value: Value<'gc>, hdl: MutateHandle<'gc, '_, A>) -> Self {
        let gc = hdl.emplace(DirectInitializer::new(value));
        Self { gc }
    }

    #[inline(always)]
    pub fn set<A: Allocator>(&self, val: Value<'gc>, hdl: MutateHandle<'gc, '_, A>) {
        self.gc.set(val);
        hdl.retrace(self.gc)
    }

    #[inline(always)]
    pub fn unwrap(self) -> Value<'gc> {
        match *self.gc {
            Value::UpValue(_) => {
                unreachable!()
            }
            t => t,
        }
    }
}

// ==========================================Value==========================================

#[mux]
#[derive(Copy, Clone, Debug)]
pub enum Value<'gc> {
    Bool(Bool),
    Integer(Integer),
    Float(Float),
    Nil(Nil),
    Vector(Vector<'gc>),
    Closure(Closure<'gc>),
    UpValue(UpValue<'gc>),
}

impl Display for Value<'_>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(t) => <Bool as Debug>::fmt(&t, f),
            Value::Integer(t) => <Integer as Debug>::fmt(&t, f),
            Value::Float(t) => <Float as Debug>::fmt(&t, f),
            Value::Nil(t) => <Nil as Debug>::fmt(&t, f),
            Value::Vector(t) => <Vector as Debug>::fmt(&t, f),
            Value::Closure(t) => <Closure as Debug>::fmt(&t, f),
            Value::UpValue(t) => <UpValue as Debug>::fmt(&t, f)
        }
    }
}

impl<'gc> Value<'gc> {
    #[inline(always)]
    pub fn is_up_value(&self)->bool{
        if let Value::UpValue(_) = self{
            true
        }else{
            false
        }
    }
    #[inline(always)]
    pub fn set(&self, val: Value<'gc>) {
        unsafe {
            *(self as *const Value<'gc> as usize as *mut Value<'gc>) = val;
        }
    }
}

impl<'gc> Collectable for Value<'gc> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        value_match!(self=>t,{
            t.trace(hdl);
        })
    }
}

// implement operations for operatees which only include simple ops without override
impl<'gc> Value<'gc> {
    #[inline(always)]
    pub fn unwrap(self) -> Self {
        if std::intrinsics::unlikely(self.is_up_value()){
            if let Value::UpValue(t) = self {
                t.unwrap()
            } else { unreachable!() }
        }else{
            self
        }

    }

    //t.data();
    #[inline(always)]
    pub fn to_bool(self) -> Result<Bool, OpError> {
        match self.unwrap() {
            Value::Bool(t) => Ok(t),
            Value::Integer(t) => Ok(t == 0),
            _ => Err(OpError::NotSupport),
        }
    }

    #[inline(always)]
    pub fn to_integer(self) -> Result<Integer, OpError> {
        match self.unwrap() {
            Value::Bool(false) => Ok(0),
            Value::Bool(true) => Ok(1),
            Value::Integer(t) => Ok(t),
            Value::Float(t) => Ok(t as _),
            _ => Err(OpError::NotSupport),
        }
    }

    #[inline(always)]
    pub fn to_float(self) -> Result<Float, OpError> {
        match self.unwrap() {
            Value::Bool(false) => Ok(0.0),
            Value::Bool(true) => Ok(1.0),
            Value::Integer(t) => Ok(t as Float),
            Value::Float(t) => Ok(t),
            _ => Err(OpError::NotSupport),
        }
    }

    #[inline(always)]
    pub fn op_or<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        use Value::*;
        let a = self.to_bool()?;
        let b = b.to_bool()?;
        Ok((a || b).into())
    }

    #[inline(always)]
    pub fn op_and<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        use Value::*;
        let a = self.to_bool()?;
        let b = b.to_bool()?;
        Ok((a && b).into())
    }

    #[inline(always)]
    pub fn op_bit_or<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        use Value::*;
        let a = self.to_integer()?;
        let b = b.to_integer()?;
        Ok((a | b).into())
    }

    #[inline(always)]
    pub fn op_bit_and<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        use Value::*;
        let a = self.to_integer()?;
        let b = b.to_integer()?;
        Ok((a & b).into())
    }

    #[inline(always)]
    pub fn op_bit_xor<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        use Value::*;
        let a = self.to_integer()?;
        let b = b.to_integer()?;
        Ok((a ^ b).into())
    }

    #[inline(always)]
    pub fn op_eq<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        match (self.unwrap(), b.unwrap()) {
            (Value::Nil(_), Value::Nil(_)) => Ok(true.into()),
            (Value::Nil(_), _) => Ok(false.into()),

            (Value::Bool(a), Value::Bool(b)) => Ok((a == b).into()),
            (Value::Bool(_), _) => Ok(false.into()),

            (Value::Integer(a), Value::Integer(b)) => Ok((a == b).into()),
            (Value::Integer(a), Value::Float(b)) => Ok((a as f64 == b).into()),
            (Value::Integer(_), _) => Ok(false.into()),

            (Value::Float(a), Value::Float(b)) => Ok((a == b).into()),
            (Value::Float(a), Value::Integer(b)) => Ok((a == b as f64).into()),
            (Value::Float(_), _) => Ok(false.into()),

            (Value::Closure(a), Value::Closure(b)) => Ok((a == b).into()),
            (Value::Closure(_), _) => Ok(false.into()),
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    pub fn op_ne<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        ok_likely!(self.op_eq(b, hdl)).op_not(hdl)
    }

    #[inline(always)]
    pub fn op_lt<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok((a < b).into())
        } else {
            Ok((ok_likely!(self.to_float()) < ok_likely!(b.to_float())).into())
        }
    }

    #[inline(always)]
    pub fn op_gt<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok((a > b).into())
        } else {
            Ok((ok_likely!(self.to_float()) > ok_likely!(b.to_float())).into())
        }
    }

    #[inline(always)]
    pub fn op_le<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok((a <= b).into())
        } else {
            Ok((ok_likely!(self.to_float()) <= ok_likely!(b.to_float())).into())
        }
    }

    #[inline(always)]
    pub fn op_ge<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok((a >= b).into())
        } else {
            Ok((ok_likely!(self.to_float()) >= ok_likely!(b.to_float())).into())
        }
    }

    #[inline(always)]
    pub fn op_l_mov<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = ok_likely!(self.to_integer());
        let b = ok_likely!(b.to_integer());
        Ok((a << b).into())
    }

    #[inline(always)]
    pub fn op_r_mov<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = ok_likely!(self.to_integer());
        let b = ok_likely!(b.to_integer());
        Ok((a >> b).into())
    }

    #[inline(always)]
    pub fn op_add<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok(Value::Integer(a + b))
        } else {
            Ok(Value::Float(
                ok_likely!(self.to_float()) + ok_likely!(b.to_float()),
            ))
        }
    }

    #[inline(always)]
    pub fn op_sub<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        self.op_add(ok_likely!(b.op_neg(hdl)), hdl)
    }

    #[inline(always)]
    pub fn op_mul<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok(Value::Integer(a * b))
        } else {
            Ok(Value::Float(self.to_float()? * b.to_float()?))
        }
    }

    #[inline(always)]
    pub fn op_div<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok(Value::Integer(a / b))
        } else {
            Ok(Value::Float(self.to_float()? / b.to_float()?))
        }
    }

    #[inline(always)]
    pub fn op_mod<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = self.to_integer()?;
        let b = b.to_integer()?;
        Ok((a % b).into())
    }

    #[inline(always)]
    pub fn op_fact<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        todo!()
    }

    #[inline(always)]
    pub fn op_bit_not<A: Allocator>(
        self,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        match self.unwrap() {
            Value::Bool(t) => Ok((!t).into()),
            t => {
                let t = t.to_integer()?;
                Ok(t.into())
            }
        }
    }

    #[inline(always)]
    pub fn op_not<A: Allocator>(
        self,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = self.to_bool()?;
        Ok((!a).into())
    }

    #[inline(always)]
    pub fn op_neg<A: Allocator>(
        self,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        match self {
            Value::Integer(t) => Ok((-t).into()),
            Value::Float(t) => Ok((-t).into()),
            Value::UpValue(a) => a.unwrap().op_neg(hdl),
            _ => Err(OpError::NotSupport),
        }
    }

    #[inline(always)]
    pub fn capture<A: Allocator>(&self, hdl: MutateHandle<'gc, '_, A>) -> UpValue<'gc> {
        let this = UpValue::new(*self, hdl);
        self.set(this.into());
        this
    }

    #[inline(always)]
    pub fn obj_clone<A: Allocator>(self, hdl: MutateHandle<'gc, '_, A>) -> Result<Self, OpError> {
        todo!()
    }
}

#[derive(Debug)]
pub enum OpError {
    NotSupport,
    IndexOutOfBounds,
}
