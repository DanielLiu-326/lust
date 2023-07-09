use crate::compiler::descriptors::{ConstantDescriptor, FnDescriptor, IdentPos, OpCodeExt};
use crate::compiler::errors::Result;
use crate::constants::Constant;
use crate::value::{FnMeta, FnProto, String};
use crate::vm::opcode::{u24, OpCode, U24};
use dst_init::SliceIterInitializer;
use gc::MutateHandle;
use std::alloc::Allocator;
use std::collections::HashMap;

pub fn generate_code(ext: Vec<OpCodeExt>) -> Result<Vec<OpCode>> {
    let mut count = 0usize;
    let mut codes: Vec<OpCode> = Vec::new();
    let mut labels: HashMap<&usize, usize> = HashMap::new();
    for x in &ext {
        match x {
            OpCodeExt::Label(label) => {
                labels.insert(label, count);
            }
            _ => count += 1,
        }
    }

    for x in &ext {
        match x {
            OpCodeExt::OpCode(op) => {
                codes.push(*op);
            }
            OpCodeExt::Goto(goto) => {
                let label_addr = labels.get(goto).unwrap();
                if label_addr > &codes.len() {
                    codes.push(OpCode::JmpPost(u24::from_u32(
                        (label_addr - codes.len() - 1) as u32,
                    )));
                } else {
                    codes.push(OpCode::JmpPrev(u24::from_u32(
                        (codes.len() - label_addr + 1) as u32,
                    )));
                }
            }
            _ => {}
        }
    }
    return Ok(codes);
}

pub fn generate_fn<'gc, A: Allocator>(
    root: FnDescriptor,
    consts: &mut Vec<Constant<'gc>>,
    codes: &mut Vec<OpCode>,
    hdl: MutateHandle<'gc, '_, A>,
) -> FnProto<'gc> {
    let pc = codes.len();
    let const_offset = consts.len();
    let mut func_codes = generate_code(root.codes.clone()).unwrap();
    codes.append(&mut func_codes);
    let mut cnt = consts.len();
    consts.resize(consts.len() + root.consts.len(), Constant::Integer(0));
    for x in root.consts {
        match x {
            ConstantDescriptor::Integer(i) => consts[cnt] = Constant::Integer(i),
            ConstantDescriptor::Float(f) => consts[cnt] = Constant::Float(f),
            ConstantDescriptor::String(s) => {
                consts[cnt] = Constant::String(String::from_str(s.as_str(), hdl))
            }
            ConstantDescriptor::Function(f) => {
                consts[cnt] = Constant::FnProto(generate_fn(f, consts, codes, hdl))
            }
        }
        cnt += 1;
    }

    let meta: FnMeta = FnMeta {
        pc,
        param_num: root.param_num,
        stack_size: 255, //TODO
        constant_offset: const_offset,
    };

    FnProto::alloc(
        meta,
        SliceIterInitializer::new(
            root.up_values.len(),
            root.up_values.iter().map(|up| match up {
                IdentPos::Register(reg) => *reg as usize,
                IdentPos::UpValue(up_val) => 256 as usize + *up_val as usize,
            }),
        ),
        hdl,
    )
}
