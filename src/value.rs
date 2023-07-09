use dst_init::macros::dst;
use dst_init::{DirectInitializer, EmplaceInitializer, RawInitializer, Slice, SliceExt};
use gc::thin::ThinInitializer;
use gc::{Collectable, Gc, MutateHandle, Thin, TraceHandle};
use macros::mux;
use std::alloc::Allocator;

use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::ptr::NonNull;

use crate::util::ok_likely;

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

#[derive(Clone, Copy)]
pub struct String<'gc> {
    gc: Gc<'gc, Thin<StringObj>>,
}

impl PartialEq for String<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<'gc> String<'gc> {
    pub fn from_str<A: Allocator>(s: &str, hdl: MutateHandle<'gc, '_, A>) -> Self {
        // TODO: Hash
        // TOOD: better copy performance
        let data = s.as_bytes();
        let init = StringObjInit {
            hash: 0,
            data: dst_init::Slice::iter_init(data.len(), data.into_iter().map(|i| *i)),
        };
        Self {
            gc: hdl.emplace(ThinInitializer::from(init)),
        }
    }

    pub fn as_str(&self) -> &str {
        unsafe { std::mem::transmute(&self.gc.data) }
    }

    pub fn add<A: Allocator>(&self, other: String<'gc>, hdl: MutateHandle<'gc, '_, A>) -> Self {
        // TOOD: better copy performance
        let self_len = self.gc.data.len();
        let other_len = other.gc.data.len();
        let res_len = self_len + other_len;
        let mut init = Slice::fn_init(res_len, || {});
        let layout = init.layout();
        let data_init = RawInitializer::new(layout, |ptr| unsafe {
            let mut write_ptr = ptr.as_ptr();
            std::ptr::copy(self.gc.data.as_ptr(), write_ptr, self_len);
            write_ptr = write_ptr.add(self_len);
            std::ptr::copy(other.gc.data.as_ptr(), write_ptr, other_len);
            return NonNull::new(std::ptr::from_raw_parts_mut(ptr.as_ptr().cast(), res_len))
                .unwrap();
        });
        Self {
            gc: hdl.emplace(ThinInitializer::from(StringObjInit {
                hash: 0,
                data: data_init,
            })),
        }
    }
}

impl<'gc> Debug for String<'gc> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str_data = std::str::from_utf8(&self.gc.deref().data).unwrap();
        <str as Debug>::fmt(str_data, f)
    }
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
        let init: FnProtoObjInit<E> = FnProtoObjInit { meta, captures };
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
#[derive(Copy, Clone, Collectable)]
pub struct Vector<'gc> {
    gc: Gc<'gc, Vec<Value<'gc>>>,
}

impl Debug for Vector<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.gc.deref())
    }
}

impl Display for Vector<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut it = self.gc.iter();
        while let Some(i) = it.next() {
            write!(f, "{}", i)?;
            if it.clone().next().is_some() {
                write!(f, ",")?;
            }
        }
        write!(f, "]")
    }
}

impl<'gc> Vector<'gc> {
    pub fn new<A: Allocator>(hdl: MutateHandle<'gc, '_, A>) -> Self {
        Vector {
            gc: hdl.create(Vec::new()),
        }
    }

    pub fn add_assign<A: Allocator>(self, rhs: Value<'gc>, _hdl: MutateHandle<'gc, '_, A>) {
        let rhs = rhs.unwrap();
        // TODO: unsafe
        unsafe {
            self.gc.clone().as_mut().push(rhs);
        }
    }

    pub fn add<A: Allocator>(self, rhs: Value<'gc>, hdl: MutateHandle<'gc, '_, A>) -> Self {
        let rhs = rhs.unwrap();
        let mut res = self.gc.deref().clone();
        res.push(rhs);
        Vector {
            gc: hdl.create(res),
        }
    }

    pub fn pop(&mut self, _val: UpValue) -> Option<Value<'gc>> {
        // TODO: unsafe
        unsafe { self.gc.as_mut().pop() }
    }

    pub fn set(&mut self, index: usize, val: Value<'gc>) {
        unsafe {
            if self.gc.as_mut().len() <= index {
                self.gc.as_mut().resize(index + 1, Value::Nil(()));
            }
            self.gc.as_mut()[index] = val;
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

    pub fn to_string<A: Allocator>(&self, hdl: MutateHandle<'gc, '_, A>) -> String<'gc> {
        // TODO: better performance
        let s = std::format!("{}", self);
        String::from_str(s.as_str(), hdl)
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

impl Debug for Closure<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure")
    }
}

impl<'gc> Collectable for Closure<'gc> {
    fn trace(&self, mut hdl: TraceHandle<'_>) {
        hdl.reached(self.gc)
    }
}

impl<'gc> Closure<'gc> {
    pub fn default<A: Allocator>(hdl: MutateHandle<'gc, '_, A>) -> Self {
        let gc: Gc<'_, Thin<ClosureObj<'_>>> = hdl.emplace(ThinInitializer::from(ClosureObjInit {
            meta: FnMeta::default(),
            up_values: Slice::iter_init(0, (0..).map(|_a| unreachable!())),
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

impl Debug for UpValue<'_> {
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
    String(String<'gc>),
    Nil(Nil),
    Vector(Vector<'gc>),
    Closure(Closure<'gc>),
    UpValue(UpValue<'gc>),
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(t) => <Bool as Display>::fmt(&t, f),
            Value::Integer(t) => <Integer as Display>::fmt(&t, f),
            Value::Float(t) => <Float as Display>::fmt(&t, f),
            Value::Nil(_) => <str as Display>::fmt("nil", f),
            Value::Vector(t) => <Vector as Display>::fmt(&t, f),
            Value::Closure(t) => <Closure as Debug>::fmt(&t, f),
            Value::UpValue(t) => <UpValue as Debug>::fmt(&t, f),
            Value::String(t) => <String<'_> as Debug>::fmt(&t, f),
        }
    }
}

impl<'gc> Value<'gc> {
    #[inline(always)]
    pub fn is_up_value(&self) -> bool {
        if let Value::UpValue(_) = self {
            true
        } else {
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
        if std::intrinsics::unlikely(self.is_up_value()) {
            if let Value::UpValue(t) = self {
                t.unwrap()
            } else {
                unreachable!()
            }
        } else {
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
    pub fn to_string<A: Allocator>(
        self,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<String<'gc>, OpError> {
        match self.unwrap() {
            Value::Bool(t) => {
                if t {
                    Ok(String::from_str("true", hdl))
                } else {
                    Ok(String::from_str("false", hdl))
                }
            }
            Value::Integer(t) => {
                // TODO: better performance
                let t_str = t.to_string();
                Ok(String::from_str(t_str.as_str(), hdl))
            }
            Value::Float(t) => {
                let t_str = t.to_string();
                Ok(String::from_str(t_str.as_str(), hdl))
            }
            Value::String(t) => Ok(t),
            Value::Nil(_) => Ok(String::from_str("nil", hdl)),
            Value::Vector(t) => {
                let s = t.to_string(hdl);
                Ok(s)
            }
            Value::Closure(_) => todo!(),
            Value::UpValue(t) => t.unwrap().to_string(hdl),
        }
    }

    #[inline(always)]
    pub fn op_or<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = self.to_bool()?;
        let b = b.to_bool()?;
        Ok((a || b).into())
    }

    #[inline(always)]
    pub fn op_and<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = self.to_bool()?;
        let b = b.to_bool()?;
        Ok((a && b).into())
    }

    #[inline(always)]
    pub fn op_bit_or<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = self.to_integer()?;
        let b = b.to_integer()?;
        Ok((a | b).into())
    }

    #[inline(always)]
    pub fn op_bit_and<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = self.to_integer()?;
        let b = b.to_integer()?;
        Ok((a & b).into())
    }

    #[inline(always)]
    pub fn op_bit_xor<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
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
        _hdl: MutateHandle<'gc, '_, A>,
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
        _hdl: MutateHandle<'gc, '_, A>,
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
        _hdl: MutateHandle<'gc, '_, A>,
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
        _hdl: MutateHandle<'gc, '_, A>,
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
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = ok_likely!(self.to_integer());
        let b = ok_likely!(b.to_integer());
        Ok((a << b).into())
    }

    #[inline(always)]
    pub fn op_r_mov<A: Allocator>(
        self,
        b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = ok_likely!(self.to_integer());
        let b = ok_likely!(b.to_integer());
        Ok((a >> b).into())
    }

    #[inline(always)]
    pub fn op_add<A: Allocator>(
        self,
        b: Value<'gc>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        if let (Value::Integer(a), Value::Integer(b)) = (self.unwrap(), b.unwrap()) {
            Ok(Value::Integer(a + b))
        } else if let (Value::String(a), b) = (self.unwrap(), b.unwrap()) {
            let b = b.to_string(hdl)?;
            Ok(a.add(b, hdl).into())
        } else if let (Value::Vector(a), b) = (self.unwrap(), b.unwrap()) {
            Ok(a.add(b, hdl).into())
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
        _hdl: MutateHandle<'gc, '_, A>,
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
        _hdl: MutateHandle<'gc, '_, A>,
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
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        let a = self.to_integer()?;
        let b = b.to_integer()?;
        Ok((a % b).into())
    }

    #[inline(always)]
    pub fn op_fact<A: Allocator>(
        self,
        _b: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        todo!()
    }

    #[inline(always)]
    pub fn op_bit_not<A: Allocator>(
        self,
        _hdl: MutateHandle<'gc, '_, A>,
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
        _hdl: MutateHandle<'gc, '_, A>,
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
    pub fn op_get_member<A: Allocator>(
        &self,
        index: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<Value<'gc>, OpError> {
        match self.unwrap() {
            Value::String(_) => todo!(),
            Value::Vector(l) => {
                if let Value::Integer(r) = index {
                    // TODO: filter i64 range
                    Ok(l.get(r.try_into().unwrap()))
                } else {
                    todo!()
                }
            }
            Value::Closure(_) => todo!(),
            _ => Err(OpError::NotSupport),
        }
    }

    #[inline(always)]
    pub fn op_set_member<A: Allocator>(
        &self,
        index: Value<'gc>,
        value: Value<'gc>,
        _hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<(), OpError> {
        match self.unwrap() {
            Value::Vector(mut l) => {
                if let Value::Integer(r) = index {
                    // TODO: filter i64 range
                    l.set(r.try_into().unwrap(), value);
                    Ok(())
                } else {
                    todo!()
                }
            }
            _ => Err(OpError::NotSupport),
        }
    }

    #[inline(always)]
    pub fn obj_clone<A: Allocator>(self, _hdl: MutateHandle<'gc, '_, A>) -> Result<Self, OpError> {
        todo!()
    }
}

#[derive(Debug)]
pub enum OpError {
    NotSupport,
    IndexOutOfBounds,
}
