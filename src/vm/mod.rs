use crate::compiler::{compile_expr_fn_call, compile_ret_stmt, compile_stmt};
use dst_init::{Slice, SliceExt};
use gc::{Collectable, GarbageCollector, GcConfig, MutateHandle, RootableTy, TraceHandle};
use std::alloc::{Allocator, Global};

use crate::constants::Constant;
use crate::util::ok_likely;
use crate::value::{self, Closure, FnProto, Nil, OpError, UpValue, Value};
use crate::vm::const_tbl::ConstTbl;
use crate::vm::error::RuntimeError;
use crate::vm::opcode::{ConstAddr, OpCode, Register, UpValueAddr, U24};
use crate::vm::stack::VmStack;

pub mod const_tbl;
mod error;
pub mod opcode;
mod stack;

struct StackFrame<'gc> {
    pc: usize,
    constant_offset: usize,
    closure: Closure<'gc>,
}

impl Collectable for StackFrame<'_> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        self.closure.trace(hdl)
    }
}

pub struct VM<'gc> {
    pc: usize,
    stack: VmStack<'gc, StackFrame<'gc>>,

    consts: ConstTbl<'gc>,
    program: Vec<OpCode>,
}

impl<'gc> Collectable for VM<'gc> {
    fn trace(&self, hdl: TraceHandle<'_>) {
        self.stack.trace(hdl);
        self.consts.trace(hdl);
    }
}

impl<'gc> VM<'gc> {
    #[inline(always)]
    pub fn new_test<A: Allocator>(
        consts: Vec<Constant<'gc>>,
        program: Vec<OpCode>,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Self {
        let mut stack = VmStack::new(
            255,
            StackFrame {
                pc: 0,
                constant_offset: 0,
                closure: Closure::default(hdl),
            },
        );
        Self {
            pc: 0,
            stack,
            consts: ConstTbl::new(consts),
            program,
        }
    }

    #[inline(always)]
    pub fn step<A: Allocator>(
        &mut self,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<(), RuntimeError> {
        unsafe {
            ok_likely!(self.execute_code(*self.program.get_unchecked(self.pc), hdl));
            self.pc += 1;
            Ok(())
        }
    }

    #[inline(always)]
    fn get_const(&mut self, addr: ConstAddr) -> Constant<'gc> {
        //TODO: unwrap error handling
        self.consts
            .get(self.stack.top().unwrap().constant_offset, addr)
    }

    #[inline(always)]
    fn get_upval(&mut self, addr: UpValueAddr) -> UpValue<'gc> {
        self.stack.top().unwrap().closure.up_values()[addr as usize]
    }

    #[inline(always)]
    fn mk_closure<A: Allocator>(
        &mut self,
        reg: Register,
        constant: ConstAddr,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<(), RuntimeError> {
        let proto: FnProto = self
            .get_const(constant)
            .try_into()
            .map_err(|_| RuntimeError::OpError(OpError::NotSupport))?; // TODO error handle
        let mut counter = 0;
        let closure = Closure::new(
            proto,
            Slice::fn_init(proto.captures().len(), || {
                let reg = proto.captures()[counter];
                counter += 1;
                if reg <= 255 {
                    // is now closure register
                    let val = self.stack.register(reg as Register).clone();
                    let upval = if let Value::UpValue(up) = val {
                        up
                    } else {
                        UpValue::new(val, hdl)
                    };
                    *self.stack.register_mut(reg as Register) = upval.into();
                    return upval;
                } else {
                    // is now closure up value
                    return self.get_upval((reg - 256) as UpValueAddr);
                }
            }),
            hdl,
        );
        if let Value::UpValue(up) = self.stack.register_mut(reg) {
            up.set(closure.into(), hdl);
        } else {
            *self.stack.register_mut(reg) = closure.into();
        }
        Ok(())
    }
    #[inline(always)]
    fn register(&self, reg: Register) -> Value<'gc> {
        self.stack.register(reg)
    }
    #[inline(always)]
    fn register_mut(&mut self, reg: Register) -> &mut Value<'gc> {
        self.stack.register_mut(reg)
    }
    #[inline(never)]
    fn anchor_add(a: Register, b: Register, c: Register) {
        println!("add")
    }
    #[inline(never)]
    fn anchor_eq() {
        println!("eq")
    }
    #[inline(never)]
    fn anchor_jmp() {
        println!("jmp")
    }
    #[inline(always)]
    fn call(
        &mut self,
        fn_val: Register,
        ret_num: Register,
        arg_num: Register,
    ) -> Result<(), RuntimeError> {
        let Value::Closure(closure) = self.register(fn_val).unwrap() else {
            let val = self.register(fn_val);
            return Err(RuntimeError::OpError(OpError::NotSupport))
        };
        //TODO register frame size
        self.stack.push_frame(
            fn_val + 1,
            255,
            StackFrame {
                pc: self.pc + 1,
                constant_offset: closure.meta().constant_offset,
                closure,
            },
        );
        self.pc = closure.meta().pc - 1;
        Ok(())
    }

    #[inline(always)]
    fn ret(&mut self) {
        self.pc = self.stack.pop_frame().unwrap().pc - 1;
    }

    #[inline(always)]
    fn execute_code<A: Allocator>(
        &mut self,
        op: OpCode,
        hdl: MutateHandle<'gc, '_, A>,
    ) -> Result<(), RuntimeError> {
        match op {
            OpCode::Or(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_or(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::And(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_and(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::BitOr(a, b, c) => {
                *self.register_mut(a) =
                    ok_likely!(self.register(b).op_bit_or(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::BitXor(a, b, c) => {
                *self.register_mut(a) =
                    ok_likely!(self.register(b).op_bit_xor(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::BitAnd(a, b, c) => {
                *self.register_mut(a) =
                    ok_likely!(self.register(b).op_bit_and(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::NE(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_ne(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::EQ(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_eq(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::RefEQ(a, b, c) => {
                todo!()
            }
            OpCode::RefNE(a, b, c) => {
                todo!()
            }
            OpCode::LT(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_lt(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::GT(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_gt(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::LE(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_le(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::GE(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_ge(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::LMov(a, b, c) => {
                *self.register_mut(a) =
                    ok_likely!(self.register(b).op_l_mov(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::RMov(a, b, c) => {
                *self.register_mut(a) =
                    ok_likely!(self.register(b).op_r_mov(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::Add(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_add(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::Sub(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_sub(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::Mul(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_mul(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::Div(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_div(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::Mod(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_mod(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::Fact(a, b, c) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_fact(self.register(c), hdl));
                hdl.root_retrace();
            }
            OpCode::BitNot(a, b) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_bit_not(hdl));
                hdl.root_retrace();
            }
            OpCode::Not(a, b) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_not(hdl));
                hdl.root_retrace();
            }
            OpCode::Neg(a, b) => {
                *self.register_mut(a) = ok_likely!(self.register(b).op_neg(hdl));
                hdl.root_retrace();
            }
            OpCode::JmpPrev(offset) => {
                self.pc -= offset.to_u32() as usize;
            }
            OpCode::JmpPost(offset) => {
                self.pc += offset.to_u32() as usize;
            }
            OpCode::Load(a, b) => {
                *self.register_mut(a) = ok_likely!(self.get_const(b).load(hdl));
                hdl.root_retrace();
            }
            OpCode::LoadUpVal(a, b) => {
                *self.register_mut(a) = self.get_upval(b).into();
            }
            OpCode::Move(a, b) => {
                let b = self.register(b);
                if let Value::UpValue(r) = self.register_mut(a) {
                    r.set(b.unwrap(), hdl);
                } else {
                    *self.register_mut(a) = b;
                }
            }
            OpCode::Copy(a, b) => {
                *self.register_mut(a) = self.stack.register(b);
            }
            OpCode::MkClosure(reg, constant) => {
                self.mk_closure(reg, constant, hdl)?;
            }
            OpCode::Ret => self.ret(),
            OpCode::TestTrue(a) => {
                if ok_likely!(self.register(a).to_bool()) {
                    self.pc += 1;
                }
            }
            OpCode::TestFalse(a) => {
                if !ok_likely!(self.register(a).to_bool()) {
                    self.pc += 1;
                }
            }
            OpCode::Call(fn_val, ret_num, arg_num) => {
                ok_likely!(self.call(fn_val, ret_num, arg_num));
            }
            OpCode::Print(t) => {
                println!("{}", self.register(t).unwrap())
            }
            OpCode::LoadNil(r) => *self.register_mut(r) = Value::Nil(()),
            OpCode::LoadEmptyVec(reg) => {
                *self.register_mut(reg) = Value::Vector(value::Vector::new(hdl))
            }
            OpCode::GetMember(dst, obj, idx) => {
                *self.register_mut(dst) =
                    self.register(obj).op_get_member(self.register(idx), hdl)?;
            }
            OpCode::SetMember(obj, idx, val) => {
                let idx = self.register(idx);
                let val = self.register(val);
                self.register_mut(obj).op_set_member(idx, val, hdl)?;
            }
            OpCode::LoadFalse(reg) => {
                *self.register_mut(reg) = Value::Bool(false);
            }
            OpCode::LoadTrue(reg) => {
                *self.register_mut(reg) = Value::Bool(true);
            }
            t => {
                println!("{:?}", t);
                todo!()
            }
        }

        Ok(())
    }
}
