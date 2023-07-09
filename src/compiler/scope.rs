use crate::compiler::descriptors::{ConstantDescriptor, IdentPos};
use crate::compiler::errors::{CompileError, Result};
use crate::util::MinimumIdAlloc;
use crate::vm::opcode::{ConstAddr, Register, UpValueAddr};
use gc::impls::impl_collect_nothing;

use std::cell::RefCell;
use std::collections::HashMap;

use std::rc::Rc;

impl_collect_nothing!(IdentPos);
type RegisterAllocator = MinimumIdAlloc<Register>;
type LabelId = usize;

#[derive(Copy, Clone)]
pub struct LoopLabel {
    pub begin: LabelId,
    pub end: LabelId,
}

struct LabelDef {
    id: LabelId,
    has_defined: bool,
}

pub struct LabelManager {
    loop_stack: Vec<LoopLabel>,
    named: HashMap<String, LabelDef>,
    allocate: LabelId,
}

impl LabelManager {
    pub fn new() -> Self {
        Self {
            loop_stack: vec![],
            named: Default::default(),
            allocate: 0,
        }
    }

    pub fn push_loop(&mut self) -> LoopLabel {
        let (begin, end) = (self.allocate, self.allocate + 1);
        self.allocate += 2;
        LoopLabel { begin, end }
    }

    pub fn now_loop(&mut self) -> Result<LoopLabel> {
        self.loop_stack
            .last()
            .map(|a| *a)
            .ok_or(CompileError::NotInLoop)
    }

    pub fn pop_loop(&mut self) -> Result<LoopLabel> {
        self.loop_stack.pop().ok_or(CompileError::NotInLoop)
    }

    pub fn def_label(&mut self, label: String) -> Result<LabelId> {
        if let Some(def) = self.named.get_mut(&label) {
            if def.has_defined {
                return Err(CompileError::LabelDefinedMultiTimes);
            }
            def.has_defined = true;
            Ok(def.id)
        } else {
            let id = self.allocate;
            self.named.insert(
                label,
                LabelDef {
                    id,
                    has_defined: true,
                },
            );
            self.allocate += 1;
            Ok(id)
        }
    }

    pub fn use_label(&mut self, label: &str) -> LabelId {
        if let Some(def) = self.named.get(label) {
            return def.id;
        }
        let id = self.allocate;
        self.allocate += 1;
        self.named.insert(
            label.to_string(),
            LabelDef {
                id,
                has_defined: false,
            },
        );
        return id;
    }

    pub fn def_anonymous(&mut self) -> LabelId {
        let ret = self.allocate;
        self.allocate += 1;
        ret
    }
}

pub trait Scope {
    fn def_variable(&mut self, ident: String) -> Result<Register>;
    fn use_ident(&mut self, ident: &str) -> Result<IdentPos>;
    fn use_const(&mut self, constant: ConstantDescriptor) -> ConstAddr;
    fn reg_alloc(&mut self) -> &mut RegisterAllocator;
    fn label_mgr(&mut self) -> Rc<RefCell<LabelManager>>;
}

pub struct FileScope {
    //TODO
}
impl FileScope {
    pub(crate) fn new() -> Self {
        Self {}
    }
}
impl Scope for FileScope {
    fn def_variable(&mut self, _ident: String) -> Result<Register> {
        todo!()
    }

    fn use_ident(&mut self, _ident: &str) -> Result<IdentPos> {
        todo!()
    }

    fn use_const(&mut self, _constant: ConstantDescriptor) -> ConstAddr {
        todo!()
    }

    fn reg_alloc(&mut self) -> &mut RegisterAllocator {
        todo!()
    }

    fn label_mgr(&mut self) -> Rc<RefCell<LabelManager>> {
        todo!()
    }
}

pub struct FunctionScope {
    parent: Rc<RefCell<dyn Scope>>,
    variable_def: HashMap<String, Register>,
    up_values: Vec<(String, IdentPos)>,
    reg_alloc: RegisterAllocator,
    consts: Vec<ConstantDescriptor>,
    label_mgr: Rc<RefCell<LabelManager>>,
}

impl FunctionScope {
    pub(crate) fn new(parent: Rc<RefCell<dyn Scope>>) -> Self {
        Self {
            parent,
            variable_def: Default::default(),
            up_values: vec![],
            reg_alloc: MinimumIdAlloc::new(0, 200),
            consts: vec![],
            label_mgr: Rc::new(RefCell::new(LabelManager::new())),
        }
    }
    pub fn consts(&self) -> &Vec<ConstantDescriptor> {
        &self.consts
    }
    pub fn up_values(&self) -> &Vec<(String, IdentPos)> {
        &self.up_values
    }
}

impl Scope for FunctionScope {
    fn def_variable(&mut self, ident: String) -> Result<Register> {
        if self.variable_def.contains_key(ident.as_str()) {
            return Err(CompileError::IdentAlreadyExists);
        }
        let reg = self.reg_alloc.allocate();
        let Some(reg) = reg else {
            return Err(CompileError::TooManyIdent);
        };

        self.variable_def.insert(ident, reg);
        Ok(reg)
    }

    fn use_ident(&mut self, ident: &str) -> Result<IdentPos> {
        if let Some(data) = self.variable_def.get(ident) {
            return Ok(IdentPos::Register(*data));
        }

        let mut i = 0;
        for (a, _) in &self.up_values {
            if a == ident {
                return Ok(IdentPos::UpValue(i));
            }
            i += 1;
        }

        let pos = self.parent.borrow_mut().use_ident(ident)?;

        self.up_values.push((ident.to_string(), pos.clone()));

        return Ok(IdentPos::UpValue((self.up_values.len() - 1) as UpValueAddr));
    }

    fn use_const(&mut self, constant: ConstantDescriptor) -> ConstAddr {
        let mut i = 0;
        for a in &self.consts {
            if *a == constant {
                return i;
            }
            i += 1;
        }
        self.consts.push(constant);
        return (self.consts.len() - 1) as ConstAddr;
    }

    fn reg_alloc(&mut self) -> &mut RegisterAllocator {
        &mut self.reg_alloc
    }

    fn label_mgr(&mut self) -> Rc<RefCell<LabelManager>> {
        self.label_mgr.clone()
    }
}

pub struct BlockScope {
    parent: Rc<RefCell<dyn Scope>>,
    variable_def: HashMap<String, Register>,
    reg_alloc: RegisterAllocator,
}

impl BlockScope {
    pub(crate) fn new(parent: Rc<RefCell<dyn Scope>>) -> Self {
        Self {
            parent: parent.clone(),
            variable_def: Default::default(),
            reg_alloc: MinimumIdAlloc::new(parent.borrow_mut().reg_alloc().top(), 200),
        }
    }
}

impl Scope for BlockScope {
    fn def_variable(&mut self, ident: String) -> Result<Register> {
        if self.variable_def.contains_key(ident.as_str()) {
            return Err(CompileError::IdentAlreadyExists);
        }
        let Some(reg) = self.reg_alloc.allocate() else {
            return Err(CompileError::TooManyIdent);
        };
        self.variable_def.insert(ident, reg);
        return Ok(reg);
    }

    fn use_ident(&mut self, ident: &str) -> Result<IdentPos> {
        if let Some(reg) = self.variable_def.get(ident) {
            return Ok(IdentPos::Register(*reg));
        }
        self.parent.borrow_mut().use_ident(ident)
    }

    fn use_const(&mut self, constant: ConstantDescriptor) -> ConstAddr {
        self.parent.borrow_mut().use_const(constant)
    }

    fn reg_alloc(&mut self) -> &mut RegisterAllocator {
        &mut self.reg_alloc
    }

    fn label_mgr(&mut self) -> Rc<RefCell<LabelManager>> {
        self.parent.borrow_mut().label_mgr()
    }
}
