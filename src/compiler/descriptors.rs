use crate::vm::opcode::{OpCode, Register, UpValueAddr};
use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::ops::{AddAssign, Deref, SubAssign};
use std::rc::Rc;
use crate::compiler::errors::CompileError;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IdentPos {
    Register(Register),
    UpValue(UpValueAddr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpCodeExt {
    OpCode(OpCode),
    Goto(usize),
    Label(usize),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExprResPos {
    New(Register),
    Source(Register),
}

impl Into<Register> for ExprResPos {
    fn into(self) -> Register {
        match self {
            ExprResPos::New(r) => r,
            ExprResPos::Source(r) => r,
        }
    }
}

#[derive(Debug)]
pub struct ExprDescriptor {
    pub res: ExprResPos,
    pub codes: Vec<OpCodeExt>,
}

#[derive(Debug)]
pub struct StmtDescriptor {
    pub codes: Vec<OpCodeExt>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FnDescriptor {
    pub consts:Vec<ConstantDescriptor>,
    pub up_values: Vec<IdentPos>,
    pub param_num: usize,
    pub codes: Vec<OpCodeExt>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantDescriptor {
    Integer(i64),
    Float(f64),
    String(String),
    Function(FnDescriptor),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LeftExprDescriptor{
    Member {
        obj:ExprResPos,
        idx:ExprResPos,
        codes:Vec<OpCodeExt>,
    },
    UpValue(UpValueAddr),
    Register(Register),
}