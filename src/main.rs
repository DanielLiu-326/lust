#![feature(ptr_metadata)]
#![feature(rustc_attrs)]
#![feature(thin_box)]
#![feature(allocator_api)]
#![feature(generators)]
#![feature(unsize)]
#![feature(layout_for_ptr)]
#![feature(alloc_layout_extra)]
#![feature(nonnull_slice_from_raw_parts)]
#![feature(decl_macro)]
#![feature(specialization)]
#![feature(const_trait_impl)]
#![feature(adt_const_params)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(const_type_id)]
#![feature(const_mut_refs)]
#![feature(const_swap)]
#![feature(generic_const_exprs)]
#![feature(unchecked_math)]
#![feature(negative_impls)]
#![feature(core_intrinsics)]
#![feature(strict_provenance)]
#![feature(auto_traits)]

use crate::constants::Constant;
use crate::value::{Integer, Value};
use crate::vm::const_tbl::ConstTbl;
use crate::vm::opcode::{u24, OpCode, U24};
use crate::vm::VM;
use gc::{Collectable, GarbageCollector, Gc, GcConfig, Rootable, TraceHandle};
use peg::Parse;
use std::alloc::Global;
use std::fmt::Debug;
use std::mem::size_of;
use std::time::Duration;

mod ast;
mod compiler;
mod constants;
mod loader;
mod saver;
mod util;
pub mod value;
mod vm;

pub struct TestObj<'gc> {
    vec: Vec<Gc<'gc, Test>>,
}

impl Collectable for TestObj<'_> {
    fn trace(&self, mut hdl: TraceHandle<'_>) {
        for x in &self.vec {
            hdl.reached(*x);
        }
    }
}

pub trait NewTrait: Collectable + Debug {}

#[derive(Debug)]
pub struct Test {
    id: u8,
}
impl Collectable for Test {
    fn trace(&self, hdl: TraceHandle<'_>) {
        println!("哼哼，啊啊啊啊啊啊啊啊啊啊，被trace力")
    }
    fn need_trace() -> bool {
        true
    }
}
impl Drop for Test {
    fn drop(&mut self) {
        println!("完了，我掉了")
    }
}

impl NewTrait for Test {}

fn main() {
    println!("opcode_size={}", size_of::<OpCode>());
    println!("register_size={}", size_of::<Value>());
    compiler::test_compiler::test();
    // let mut op_codes = Vec::new();
    // op_codes.push(OpCode::Load(1, 0));
    // op_codes.push(OpCode::Load(2, 1));
    // op_codes.push(OpCode::Load(3, 2));
    //
    // op_codes.push(OpCode::EQ(0, 2, 1));
    // op_codes.push(OpCode::TestFalse(0));
    // op_codes.push(OpCode::JmpPost(u24::from_u32(2)));
    // op_codes.push(OpCode::Add(1, 1, 3));
    // op_codes.push(OpCode::JmpPrev(u24::from_u32(5)));
    // op_codes.push(OpCode::Ret(0));
    // let mut const_table = Vec::new();
    // const_table.push(Constant::Integer(Integer::from(0)));
    // const_table.push(Constant::Integer(Integer::from(1_000_000_000)));
    // const_table.push(Constant::Integer(Integer::from(1)));
    // ConstTbl::new(const_table);
    // let mut vm = VM::new_test(const_table, op_codes, );
    // vm.run();
}
