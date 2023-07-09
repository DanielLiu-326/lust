use std::fmt::{Debug, Formatter};
use std::mem;

pub type Register = u8;
pub type ConstAddr = u16;
pub type UpValueAddr = u16;

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq)]
pub struct u24([u8; 3]);
impl Debug for u24 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_u32())
    }
}
///65535
pub type NumLiteral = [u8; 3];

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    Or(Register, Register, Register),
    And(Register, Register, Register),
    BitOr(Register, Register, Register),
    BitXor(Register, Register, Register),
    BitAnd(Register, Register, Register),
    NE(Register, Register, Register),
    EQ(Register, Register, Register),
    RefEQ(Register, Register, Register),
    RefNE(Register, Register, Register),
    LT(Register, Register, Register),
    GT(Register, Register, Register),
    LE(Register, Register, Register),
    GE(Register, Register, Register),
    LMov(Register, Register, Register),
    RMov(Register, Register, Register),
    Add(Register, Register, Register),
    Sub(Register, Register, Register),
    Mul(Register, Register, Register),
    Div(Register, Register, Register),
    Mod(Register, Register, Register),
    Fact(Register, Register, Register),

    BitNot(Register, Register),
    Not(Register, Register),
    Neg(Register, Register),

    JmpPrev(u24),
    JmpPost(u24),

    Load(Register, ConstAddr),
    LoadEmptyVec(Register),
    LoadNil(Register),
    LoadFalse(Register),
    LoadTrue(Register),
    LoadUpVal(Register, UpValueAddr),
    Move(Register, Register),
    Copy(Register, Register),
    //LoadI(NumLiteral),
    MkClosure(Register, ConstAddr),
    Call(Register, Register, Register), // callee,ret_num,arg_num
    Ret,
    SetMember(Register, Register, Register),
    GetMember(Register, Register, Register),

    TestFalse(Register), // if R is false then pc + 1
    TestTrue(Register),  // if R is true then pc +1
    Print(Register),     //TODO Temporary.
}

pub trait U24 {
    fn to_u32(&self) -> u32;
    fn from_u32(val: u32) -> Self;
}

impl U24 for u24 {
    #[cfg(target_endian = "little")]
    #[inline(always)]
    fn to_u32(&self) -> u32 {
        unsafe {
            let mut a: [u8; 4] = mem::MaybeUninit::zeroed().assume_init();
            a[0] = self.0[0];
            a[1] = self.0[1];
            a[2] = self.0[2];
            a[3] = 0;
            return mem::transmute(a);
        }
    }

    #[cfg(target_endian = "little")]
    #[inline(always)]
    fn from_u32(val: u32) -> Self {
        // FIXME: assert
        unsafe {
            let val: [u8; 4] = mem::transmute(val);
            Self([val[0], val[1], val[2]])
        }
    }
}
// chunk
//      consts
//      protos
//
