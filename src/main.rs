#![feature(ptr_metadata)]
#![feature(rustc_attrs)]
#![feature(thin_box)]
#![feature(allocator_api)]
#![feature(generators)]
#![feature(unsize)]
#![feature(layout_for_ptr)]
#![feature(alloc_layout_extra)]
#![feature(decl_macro)]
#![feature(const_trait_impl)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(const_type_id)]
#![feature(const_mut_refs)]
#![feature(const_swap)]
#![feature(unchecked_math)]
#![feature(negative_impls)]
#![feature(core_intrinsics)]
#![feature(strict_provenance)]
#![feature(auto_traits)]

use crate::value::Value;
use crate::vm::opcode::OpCode;

use std::mem::size_of;

mod ast;
mod compiler;
mod constants;
mod loader;
mod saver;
mod util;
pub mod value;
mod vm;

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
