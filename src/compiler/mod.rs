use crate::ast::{BinaryOp, Expr, Ident, LeftExpr, Literal, Stmt};
use crate::compiler::descriptors::{ConstantDescriptor, ExprDescriptor, ExprResPos, FnDescriptor, IdentPos, LeftExprDescriptor, OpCodeExt, StmtDescriptor};
use crate::compiler::errors::{CompileError, Result};
use crate::compiler::scope::{BlockScope, FileScope, FunctionScope, Scope};
use crate::vm::opcode::{OpCode, Register, u24, U24};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsString;
use std::intrinsics::rotate_left;
use std::mem::needs_drop;
use std::ops::{Deref, DerefMut};
use std::ptr::from_raw_parts;
use std::rc::Rc;
use std::str::FromStr;
use crate::compiler::linker::generate_fn;

use self::descriptors::VectorDescriptor;

mod descriptors;
mod errors;
mod linker;
mod parser;
mod scope;

pub fn compile_expr_ident(
    parent_scope: Rc<RefCell<dyn Scope>>,
    ident: Ident,
    advice: Option<Register>,
) -> Result<ExprDescriptor> {
    let ident_pos = parent_scope.borrow_mut().use_ident(ident)?;
    let mut codes = Vec::new();
    let res;
    match ident_pos {
        IdentPos::Register(r) => res = ExprResPos::Source(r),
        IdentPos::UpValue(up) => {
            if let Some(r) = advice {
                res = ExprResPos::Source(r);
            } else {
                res = ExprResPos::New(
                    parent_scope
                        .borrow_mut()
                        .reg_alloc()
                        .allocate()
                        .ok_or(CompileError::TooManyIdent)?,
                );
            }
            codes.push(OpCodeExt::OpCode(OpCode::LoadUpVal(res.into(), up)))
        }
    }
    Ok(ExprDescriptor { res, codes })
}

pub fn binary_op_to_opcode(
    parent_scope: Rc<RefCell<dyn Scope>>,
    res: Register,
    op: BinaryOp,
    sub_res: (Register, Register),
    mut opcodes: (Vec<OpCodeExt>, Vec<OpCodeExt>),
) -> Result<Vec<OpCodeExt>> {
    let mut codes = Vec::new();
    codes.append(&mut opcodes.0);
    codes.append(&mut opcodes.1);
    match op {
        BinaryOp::OpBitOr => codes.push(OpCodeExt::OpCode(OpCode::BitOr(res, sub_res.0, sub_res.1))),
        BinaryOp::OpBitXor => codes.push(OpCodeExt::OpCode(OpCode::BitXor(res, sub_res.0, sub_res.1))),
        BinaryOp::OpBitAnd => codes.push(OpCodeExt::OpCode(OpCode::BitAnd(res, sub_res.0, sub_res.1))),
        BinaryOp::OpNe => codes.push(OpCodeExt::OpCode(OpCode::NE(res, sub_res.0, sub_res.1))),
        BinaryOp::OpEq => codes.push(OpCodeExt::OpCode(OpCode::EQ(res, sub_res.0, sub_res.1))),
        BinaryOp::OpRefEq => codes.push(OpCodeExt::OpCode(OpCode::RefEQ(res, sub_res.0, sub_res.1))),
        BinaryOp::OpRefNe => codes.push(OpCodeExt::OpCode(OpCode::RefNE(res, sub_res.0, sub_res.1))),
        BinaryOp::OpLt => codes.push(OpCodeExt::OpCode(OpCode::LT(res, sub_res.0, sub_res.1))),
        BinaryOp::OpGt => codes.push(OpCodeExt::OpCode(OpCode::GT(res, sub_res.0, sub_res.1))),
        BinaryOp::OpLe => codes.push(OpCodeExt::OpCode(OpCode::LE(res, sub_res.0, sub_res.1))),
        BinaryOp::OpGe => codes.push(OpCodeExt::OpCode(OpCode::GE(res, sub_res.0, sub_res.1))),
        BinaryOp::OpBitLMov => codes.push(OpCodeExt::OpCode(OpCode::LMov(res, sub_res.0, sub_res.1))),
        BinaryOp::OpBitRMov => codes.push(OpCodeExt::OpCode(OpCode::RMov(res, sub_res.0, sub_res.1))),
        BinaryOp::OpAdd => codes.push(OpCodeExt::OpCode(OpCode::Add(res, sub_res.0, sub_res.1))),
        BinaryOp::OpSub => codes.push(OpCodeExt::OpCode(OpCode::Sub(res, sub_res.0, sub_res.1))),
        BinaryOp::OpMult => codes.push(OpCodeExt::OpCode(OpCode::Mul(res, sub_res.0, sub_res.1))),
        BinaryOp::OpDiv => codes.push(OpCodeExt::OpCode(OpCode::Div(res, sub_res.0, sub_res.1))),
        BinaryOp::OpMod => codes.push(OpCodeExt::OpCode(OpCode::Mod(res, sub_res.0, sub_res.1))),
        BinaryOp::OpFact => codes.push(OpCodeExt::OpCode(OpCode::Fact(res, sub_res.0, sub_res.1))),
        _=>{
            println!("{:?}",op);
            unreachable!()
        }
    }
    Ok(codes)
}

pub fn compile_expr_binary_logic(
    parent_scope: Rc<RefCell<dyn Scope>>,
    l: Expr,
    op: BinaryOp,
    r: Expr,
    advice: Option<Register>,
) -> Result<ExprDescriptor> {
    let mut codes = Vec::new();
    let tmp = parent_scope.borrow_mut().reg_alloc().allocate().unwrap();// TODO: unwrap
    if let BinaryOp::OpAnd = op{
        codes.push(OpCodeExt::OpCode(OpCode::LoadFalse(tmp)));
        let ExprDescriptor {
            res: mut l_res,
            codes: mut l_opcode,
        } = compile_expr(parent_scope.clone(), l, Some(tmp))?;
        let label = parent_scope.borrow_mut().label_mgr().borrow_mut().def_anonymous();
        codes.push(OpCodeExt::OpCode(OpCode::TestTrue(l_res.into())));
        codes.push(OpCodeExt::Goto(label));
        codes.append(&mut l_opcode);
        let ExprDescriptor {
            res: mut r_res,
            codes: mut r_opcode,
        } = compile_expr(parent_scope.clone(), r, Some(tmp))?;
        if tmp != r_res.into() {
            codes.push(OpCodeExt::OpCode(OpCode::Move(tmp, r_res.into())))
        } 
        codes.push(OpCodeExt::Label(label));
        return Ok(ExprDescriptor { res: ExprResPos::New(tmp), codes });
    }else if let BinaryOp::OpOr = op{
        codes.push(OpCodeExt::OpCode(OpCode::LoadTrue(tmp)));
        let ExprDescriptor {
            res: mut l_res,
            codes: mut l_opcode,
        } = compile_expr(parent_scope.clone(), l, Some(tmp))?;
        let label = parent_scope.borrow_mut().label_mgr().borrow_mut().def_anonymous();
        codes.push(OpCodeExt::OpCode(OpCode::TestFalse(l_res.into())));
        codes.push(OpCodeExt::Goto(label));
        codes.append(&mut l_opcode);
        let ExprDescriptor {
            res: mut r_res,
            codes: mut r_opcode,
        } = compile_expr(parent_scope.clone(), r, Some(tmp))?;
        if tmp != r_res.into(){
            codes.push(OpCodeExt::OpCode(OpCode::Move(tmp, r_res.into())))
        } 
        codes.push(OpCodeExt::Label(label));
        return Ok(ExprDescriptor { res: ExprResPos::New(tmp), codes });
    }else{
        unreachable!()
    }
}

pub fn compile_expr_binary(
    parent_scope: Rc<RefCell<dyn Scope>>,
    l: Expr,
    op: BinaryOp,
    r: Expr,
    advice: Option<Register>,
) -> Result<ExprDescriptor> {
    if let BinaryOp::OpAnd|BinaryOp::OpOr = op{
        return compile_expr_binary_logic(parent_scope, l, op, r, advice);
    }
    let ExprDescriptor {
        res: mut l_res,
        codes: mut l_opcode,
    } = compile_expr(parent_scope.clone(), l, None)?;
    let ExprDescriptor {
        res: mut r_res,
        codes: mut r_opcode,
    } = compile_expr(parent_scope.clone(), r, None)?;
    // allocate a register for result
    let res = if let Some(reg) = advice {
        ExprResPos::Source(reg)
    } else {
        let reg = if let ExprResPos::New(reg) = l_res {
            // no longer needs to deallocate
            l_res = ExprResPos::Source(reg);
            reg
        } else if let ExprResPos::New(reg) = r_res {
            // no longer needs to deallocate
            r_res = ExprResPos::Source(reg);
            reg
        } else {
            parent_scope
                .borrow_mut()
                .reg_alloc()
                .allocate()
                .ok_or(CompileError::TooManyIdent)?
        };
        ExprResPos::New(reg)
    };
    let codes = binary_op_to_opcode(
        parent_scope.clone(),
        res.into(),
        op,
        (l_res.into(), r_res.into()),
        (l_opcode, r_opcode),
    )?;
    // clean temporary value
    if let ExprResPos::New(reg) = l_res {
        parent_scope.borrow_mut().reg_alloc().recycle(reg);
    }
    if let ExprResPos::New(reg) = r_res {
        parent_scope.borrow_mut().reg_alloc().recycle(reg);
    }
    Ok(ExprDescriptor { res, codes })
}

pub fn compile_fn_literal(parent_scope: Rc<RefCell<dyn Scope>>, params:Vec<Ident>, body:Vec<Stmt>)
    -> Result<FnDescriptor>
{
    let mut scope = Rc::new(RefCell::new(FunctionScope::new(parent_scope)));
    for param in &params{
        // reg is allocate from 0 increase
        let reg = scope.borrow_mut().def_variable(param.to_string())?;
    }

    let mut codes = compile_stmts(scope.clone(), body)?;
    codes.push(OpCodeExt::OpCode(OpCode::Ret)); // TODO: return nil
    // TODO: check return stmt exists
    let consts = scope.borrow_mut().consts().clone();
    let up_values = scope.borrow_mut().up_values().iter().map(|(_,id)|{*id}).collect();
    Ok(FnDescriptor{
        consts,
        up_values ,
        param_num: params.len(),
        codes,
    })
}

pub fn compile_vector_literal(
    parent_scope: Rc<RefCell<dyn Scope>>,
    members:Vec<Expr>,
    reg: Register,
)->Result<VectorDescriptor>{
    let mut codes = Vec::new();
    codes.push(OpCodeExt::OpCode(OpCode::LoadEmptyVec(reg)));
    for mem in members{
        let scope = Rc::new(RefCell::new(BlockScope::new(parent_scope.clone())));
        let ExprDescriptor{res:expr_res,codes:mut expr_codes} = compile_expr(scope, mem, None)?;
        codes.append(&mut expr_codes);
        codes.push(OpCodeExt::OpCode(OpCode::Add(reg, reg, expr_res.into())));
    }
    return Ok(VectorDescriptor {
        res:reg,
        codes,
    });
}

pub fn compile_expr_literal(
    parent_scope: Rc<RefCell<dyn Scope>>,
    literal: Literal,
    advice: Option<Register>,
) -> Result<ExprDescriptor> {
    let mut codes: Vec<OpCodeExt> = Vec::new();
    // allocate register for target
    let res = if let Some(reg) = advice {
        ExprResPos::Source(reg)
    } else {
        let reg = parent_scope
            .borrow_mut()
            .reg_alloc()
            .allocate()
            .ok_or(CompileError::TooManyIdent)?;
        ExprResPos::New(reg)
    };
    // for each type of value
    match literal {
        Literal::Nil => {
            codes.push(OpCodeExt::OpCode(OpCode::LoadNil(res.into())));
        }
        Literal::Float(f) => {
            let addr = parent_scope
                .borrow_mut()
                .use_const(ConstantDescriptor::Float(f));
            codes.push(OpCodeExt::OpCode(OpCode::Load(res.into(), addr)));
        }
        Literal::Integer(int) => {
            let addr = parent_scope
                .borrow_mut()
                .use_const(ConstantDescriptor::Integer(int));
            codes.push(OpCodeExt::OpCode(OpCode::Load(res.into(), addr)))
        }
        Literal::Dict(_) => {
            todo!()
        }
        Literal::Bool(bool) => {
            if bool {
                codes.push(OpCodeExt::OpCode(OpCode::LoadTrue(res.into())))
            } else {
                codes.push(OpCodeExt::OpCode(OpCode::LoadFalse(res.into())))
            }
        }
        Literal::String(string) => {
            let addr = parent_scope
                .borrow_mut()
                .use_const(ConstantDescriptor::String(string.to_string()));
            codes.push(OpCodeExt::OpCode(OpCode::Load(res.into(), addr)))
        }
        Literal::Function { params, body } => {
            let desc = compile_fn_literal(parent_scope.clone(), params, body)?;
            let addr = parent_scope
                .borrow_mut()
                .use_const(ConstantDescriptor::Function(desc));
            codes.push(OpCodeExt::OpCode(OpCode::MkClosure(res.into(), addr)))
        }
        Literal::Vector(v) => {
            let desc = compile_vector_literal(parent_scope, v, res.into())?;
            codes = desc.codes;
        },
    }
    Ok(ExprDescriptor { res, codes })
}

pub fn compile_expr_fn_call(parent_scope:Rc<RefCell<dyn Scope>>, callee:Expr, args:Vec<Expr>)
    -> Result<ExprDescriptor>
{
    //TODO： temporary print implementation
    {
        if let Expr::Ident("print") = callee {
            let arg = args.into_iter().next().unwrap();
            let ExprDescriptor { res, mut codes } = compile_expr(parent_scope.clone(), arg, None)?;
            codes.push(OpCodeExt::OpCode(OpCode::Print(res.into())));
            return Ok(ExprDescriptor {
                res,
                codes,
            })
        }
    }

    let arg_num = args.len();
    let mut start = parent_scope.borrow_mut().reg_alloc().allocate_top().ok_or(CompileError::TooManyIdent)?;
    for _ in &args{
        parent_scope.borrow_mut().reg_alloc().allocate_top().ok_or(CompileError::TooManyIdent)?;
    }
    let end = start + args.len() as u8;
    let ExprDescriptor{ res, mut codes }
        = compile_expr(parent_scope.clone(), callee, Some(start))?;
    let mut callee_reg= res.into();
    let mut now = start;
    if start != res.into()&&start - 1 != res.into() {
        now = start + 1;
        callee_reg = start;
        codes.push(OpCodeExt::OpCode(OpCode::Move(start, res.into())));
    }else if start == res.into(){
        now = start + 1;
    }


    let ret_reg = callee_reg + 1;
    for x in args{
        let ExprDescriptor{ res:arg_res, codes:mut arg_codes }
            = compile_expr(parent_scope.clone(), x, Some(now))?;
        codes.append(&mut arg_codes);
        if <ExprResPos as Into<Register>>::into(arg_res) != now{
            codes.push(OpCodeExt::OpCode(OpCode::Move(now, arg_res.into())));
        }
        now += 1;
    }
    codes.push(OpCodeExt::OpCode(OpCode::Call(callee_reg, arg_num as Register, 1)));
    for i in start..end {
        if ret_reg==i{
            continue;
        }
        parent_scope.borrow_mut().reg_alloc().recycle(i);
    }
    Ok(ExprDescriptor{
        res:ExprResPos::New(ret_reg),
        codes,
    })
}

pub fn compile_expr_index_visit(parent_scope: Rc<RefCell<dyn Scope>>, left:Expr, index:Expr)
    -> Result<ExprDescriptor>
{
    let LeftExprDescriptor::Member{ obj, idx, mut codes } = compile_left_expr_member(parent_scope.clone(), left, index)?else{
        unreachable!()
    };

    let res = if let ExprResPos::New(reg) = obj{
        if let ExprResPos::New(idx) = idx{
            parent_scope.borrow_mut().reg_alloc().recycle(idx)
        }
        reg
    }else if let ExprResPos::Source(reg) = idx{
        reg
    }else{
        parent_scope.borrow_mut().reg_alloc().allocate().unwrap() // TODO unwrap
    };

    codes.push(OpCodeExt::OpCode(OpCode::GetMember(res, obj.into(), idx.into())));
    Ok(ExprDescriptor { res: ExprResPos::New(res), codes })
}

pub fn compile_expr(
    parent_scope: Rc<RefCell<dyn Scope>>,
    expr: Expr,
    advice: Option<Register>,
) -> Result<ExprDescriptor> {
    match expr {
        Expr::Ident(id) => compile_expr_ident(parent_scope, id, advice),
        Expr::BinaryOp { left, op, right } => {
            compile_expr_binary(parent_scope, *left, op, *right, advice)
        }
        Expr::UnaryOp { expr, op } => todo!(),
        Expr::FnCall { callee, args }
            => compile_expr_fn_call(parent_scope, *callee, args),
        Expr::IndexVisit { expr, index } 
            => compile_expr_index_visit(parent_scope, *expr, *index),
        Expr::Brace(_) => {
            todo!()
        }
        Expr::Literal(lit) => compile_expr_literal(parent_scope, lit, advice),
    }
}

pub fn compile_left_expr_ident(parent_scope: Rc<RefCell<dyn Scope>>, id:Ident)
    -> Result<LeftExprDescriptor>
{
    let pos = parent_scope.borrow_mut().use_ident(id)?;
    match pos{
        IdentPos::Register(reg) => Ok(LeftExprDescriptor::Register(reg)),
        IdentPos::UpValue(up) => Ok(LeftExprDescriptor::UpValue(up))
    }
}

pub fn compile_left_expr_member(parent_scope: Rc<RefCell<dyn Scope>>, prefix:Expr, index:Expr)
    -> Result<LeftExprDescriptor>
{
    let mut codes = Vec::new();
    let ExprDescriptor{res:prefix_res, codes:mut prefix_codes}
        = compile_expr(parent_scope.clone(), prefix, None)?;
    codes.append(&mut prefix_codes);
    let ExprDescriptor{res:index_res, codes:mut index_codes}
        = compile_expr(parent_scope.clone(), index, None)?;
    codes.append(&mut index_codes);
    Ok(LeftExprDescriptor::Member {
        obj: prefix_res,
        idx: index_res,
        codes, 
    })
}

pub fn compile_left_expr(parent_scope: Rc<RefCell<dyn Scope>>, expr:LeftExpr)
    -> Result<LeftExprDescriptor>
{
    match expr{
        LeftExpr::Ident(id)
            => compile_left_expr_ident(parent_scope, id),
        LeftExpr::Member { prefix,index }
            => compile_left_expr_member(parent_scope, prefix, index),
    }
}

pub fn compile_var_define_stmt(
    parent_scope: Rc<RefCell<dyn Scope>>,
    ident: Ident,
    right: Expr,
) -> Result<StmtDescriptor> {
    let reg = parent_scope.borrow_mut().def_variable(ident.to_string())?;
    let ExprDescriptor { res, mut codes }
        = compile_expr(parent_scope.clone(), right, Some(reg))?;
    // if the expr compile process reject our target advice.
    if reg != res.into() {
        codes.push(OpCodeExt::OpCode(OpCode::Move(reg, res.into())));
    }
    // deallocate registers allocated by expression
    if let ExprResPos::New(reg) = res {
        parent_scope.borrow_mut().reg_alloc().recycle(reg);
    }

    Ok(StmtDescriptor { codes })
}

pub fn compile_if_stmt(
    parent_scope: Rc<RefCell<dyn Scope>>,
    cond: Expr,
    success: Stmt,
    failure: Option<Stmt>,
) -> Result<StmtDescriptor> {
    let ExprDescriptor{res:cond_res, codes: mut codes }
        = compile_expr(parent_scope.clone(), cond, None)?;
    let mut fail_codes = if let Some(stmt) = failure{
        compile_stmt(parent_scope.clone(), stmt)?.codes
    }else{
        Vec::new()
    };
    let mut success_codes = compile_stmt(parent_scope.clone(), success)?.codes;
    /// cond
    /// chk
    /// jmp
    /// fail
    /// jmp
    /// success
    let success_start = parent_scope.borrow_mut().label_mgr().borrow_mut().def_anonymous();
    let success_end = parent_scope.borrow_mut().label_mgr().borrow_mut().def_anonymous();
    fail_codes.push(OpCodeExt::Goto(success_end));
    codes.push(OpCodeExt::OpCode(OpCode::TestFalse(cond_res.clone().into())));
    codes.push(OpCodeExt::Goto(success_start));
    codes.append(&mut fail_codes);

    codes.push(OpCodeExt::Label(success_start));
    codes.append(&mut success_codes);
    codes.push(OpCodeExt::Label(success_end));

    // clean the temporary register
    if let ExprResPos::New(reg) = cond_res{
        parent_scope.borrow_mut().reg_alloc().recycle(reg);
    }
    // return value
    Ok(StmtDescriptor{
        codes,
    })
}

pub fn compile_block_stmt(parent_scope: Rc<RefCell<dyn Scope>>, stmts:Vec<Stmt>)
    -> Result<StmtDescriptor>
{
    let scope = Rc::new(RefCell::new(BlockScope::new(parent_scope)));
    let mut codes = Vec::new();
    for stmt in stmts{
        let mut res = compile_stmt(scope.clone(), stmt)?;
        codes.append(&mut res.codes);
    }

    Ok(StmtDescriptor{
        codes
    })
}

pub fn compile_while_stmt(parent_scope: Rc<RefCell<dyn Scope>>, cond: Expr, body: Stmt)
    -> Result<StmtDescriptor>
{
    let mut codes = Vec::new();
    let loop_label = parent_scope.borrow_mut()
        .label_mgr().borrow_mut()
        .push_loop();
    codes.push(OpCodeExt::Label(loop_label.begin));
    let ExprDescriptor{res:cond_res, codes:mut cond_codes}
        = compile_expr(parent_scope.clone(), cond, None)?;
    codes.append(&mut cond_codes);
    // condition check
    codes.push(OpCodeExt::OpCode(OpCode::TestTrue(cond_res.into())));
    codes.push(OpCodeExt::Goto(loop_label.end));
    let StmtDescriptor{codes: mut body_codes}
        = compile_stmt(parent_scope, body, )?;
    codes.append(&mut body_codes);
    codes.push(OpCodeExt::Goto(loop_label.begin));
    codes.push(OpCodeExt::Label(loop_label.end));
    Ok(StmtDescriptor{
        codes,
    })
}

pub fn compile_assign_stmt(parent_scope: Rc<RefCell<dyn Scope>>, left: LeftExpr, right: Expr)
    -> Result<StmtDescriptor>
{
    let mut codes = Vec::new();
    let left_desc = compile_left_expr(parent_scope.clone(), left)?;
    match left_desc{
        LeftExprDescriptor::Member { obj, idx, codes:mut expr_codes } => {
            codes.append(&mut expr_codes);
            let ExprDescriptor{ res:right_res, codes:mut right_codes } 
                = compile_expr(parent_scope.clone(), right, None)?;
            codes.append(&mut right_codes);
            codes.push(OpCodeExt::OpCode(OpCode::SetMember(obj.into(), idx.into(), right_res.into())));
            // TODO: clean the unused registers
            Ok(StmtDescriptor { codes })
        },
        LeftExprDescriptor::Register (reg) => {
            let mut right_res = compile_expr(parent_scope, right, Some(reg))?;
            codes.append(&mut right_res.codes);
            if <ExprResPos as Into<Register>>::into(right_res.res) != reg {
                codes.push(OpCodeExt::OpCode(OpCode::Move(reg, right_res.res.into())))
            }
            Ok(StmtDescriptor{
                codes,
            })
        }
        LeftExprDescriptor::UpValue(_) => todo!(),
    }
}

pub fn compile_ret_stmt(parent_scope: Rc<RefCell<dyn Scope>>, expr:Expr) -> Result<StmtDescriptor>{
    let mut codes = Vec::new();
    let ExprDescriptor{res:expr_reg,codes:mut expr_codes} = compile_expr(parent_scope.clone(), expr,None)?;
    codes.append(&mut expr_codes);
    codes.push(OpCodeExt::OpCode(OpCode::Move(0,expr_reg.into())));
    codes.push(OpCodeExt::OpCode(OpCode::Ret));
    // recycle the registers allocated by expression;
    if let ExprResPos::New(reg) = expr_reg{
        parent_scope.borrow_mut().reg_alloc().recycle(reg);
    }
    Ok(StmtDescriptor{
        codes,
    })
}

pub fn compile_normal_stmt(parent_scope: Rc<RefCell<dyn Scope>>, expr:Expr)
    -> Result<StmtDescriptor>
{
    let ExprDescriptor{ res, codes } = compile_expr(parent_scope.clone(), expr, None)?;
    if let ExprResPos::New(r) = res{
        parent_scope.borrow_mut().reg_alloc().recycle(r);
    }
    Ok(StmtDescriptor{
        codes,
    })
}

pub fn compile_stmt(parent_scope: Rc<RefCell<dyn Scope>>, stmt: Stmt) -> Result<StmtDescriptor> {
    match stmt {
        Stmt::IfStmt {
            cond,
            success,
            failure,
        } => compile_if_stmt(parent_scope, cond, *success, failure.map(|a|{*a})),
        Stmt::BreakStmt => { todo!() }
        Stmt::ContinueStmt => { todo!() }
        Stmt::AssignStmt { left, right } => compile_assign_stmt(parent_scope, left,right),
        Stmt::VarDefineStmt { ident, right }
            => compile_var_define_stmt(parent_scope, ident, right),
        Stmt::ConstDefineStmt { .. } => { todo!() }
        Stmt::NormalStmt(expr)
            => compile_normal_stmt(parent_scope,expr),
        Stmt::WhileStmt { cond, body }
            => compile_while_stmt(parent_scope, cond, *body),
        Stmt::LoopStmt(_) => {
            todo!()
        }
        Stmt::BlockStmt(block)
            =>compile_block_stmt(parent_scope, block),
        Stmt::ReturnStmt(expr)
            =>compile_ret_stmt(parent_scope, expr),
        Stmt::Label(_, _) => {
            todo!()
        }
    }
}

pub fn compile_stmts(
    parent_scope: Rc<RefCell<dyn Scope>>,
    stmts: Vec<Stmt>,
) -> Result<Vec<OpCodeExt>> {
    let mut ret = Vec::new();
    for stmt in stmts {
        let res = compile_stmt(parent_scope.clone(), stmt)?;
        for x in res.codes {
            ret.push(x);
        }
    }
    return Ok(ret);
}

pub fn generate_code(ext:Vec<OpCodeExt>) -> Result<Vec<OpCode>>{
    let mut count = 0usize;
    let mut codes = Vec::new();
    let mut labels = HashMap::new();
    for x in &ext{
        match x {
            OpCodeExt::Label(label) => {
                labels.insert(label, count);
            }
            _=>{ count += 1 }
        }
    }

    for x in &ext {
        match x{
            OpCodeExt::OpCode(op) => {
                codes.push(*op);
            }
            OpCodeExt::Goto(goto) => {
                let label_addr = labels.get(goto).unwrap();
                if label_addr > &codes.len(){
                    codes.push(OpCode::JmpPost(u24::from_u32((label_addr - codes.len() - 1) as u32)));
                }else{
                    codes.push(OpCode::JmpPrev(u24::from_u32((codes.len() - label_addr + 1) as u32)));
                }
            }
            _=>{}
        }
    }
    return Ok(codes)
}

//#[cfg(test)]
pub mod test_compiler{
    use std::alloc::Global;
    use std::cell::RefCell;
    use std::rc::Rc;
    use gc::{GarbageCollector, RootableTy, impls::*, GcConfig};
    use crate::compiler::{compile_fn_literal, parser};
    use crate::compiler::linker::generate_fn;
    use crate::compiler::scope::FileScope;
    use crate::constants::Constant;
    use crate::vm::VM;

    //#[test]
    pub(crate) fn test() {
        //let end = 1000000000;
        r#"            
        let fibonacci = nil;
        fibonacci = fn(a){
            if(a<2){
                return 1;
            }else{
                return fibonacci(a-1)+ fibonacci(a-2);
            }
        };
        let i = 0;
        while(i<30){
           print(fibonacci(i));
            i = i+1;
        }"#;
        r#"let unsorted = [6,8,1,4,3,9,8,2,1,7,4,5,3];
        let quick_sort = nil;
        quick_sort = fn(vec, left, right){
            if(right <= left){
                return nil ;
            }
            let pivot = vec[0];
            let l = left;
            let r = right;
            while(l<r){
                while( l<r && vec[r]>pivot ) r = r - 1;
                vec[l] = vec[r];
                while( l<r && vec[l]<pivot ) l = l + 1;
                vec[r] = vec[l];
            }
            vec[l] = pivot;
            quick_sort(vec, left, l);
            quick_sort(vec, l+1, right);
        };
        print(quick_sort(unsorted, 0, 13));"#;
        let src = r#"
        let test = fn(out){
            print("adsf");
            return test;
        };
        test(true)&&test(false);
"#;
        let ast = parser::src_file(src).unwrap();
        println!("============AST============");
        println!("{:?}", ast);

        println!("=========OpCodeExt=========");
        let file_scope: Rc<RefCell<FileScope>> = Rc::new(RefCell::new(FileScope::new()));
        let func: crate::compiler::descriptors::FnDescriptor = compile_fn_literal(file_scope, Vec::new(), ast.stmts).unwrap();
        println!("compile result");
        println!("constant table:{:?}", func.consts);
        println!("extended opcode:{:?}", func.codes);

        println!("===========OpCode==========");
        let mut gc:GarbageCollector<Global, RootableTy![VM<'gc>]> = GarbageCollector::new(Global, GcConfig::default(), |hdl| {
            let mut consts: Vec<Constant<'_>> = Vec::new();
            let mut codes: Vec<crate::vm::opcode::OpCode> = Vec::new();
            let func = generate_fn(func, &mut consts, &mut codes, hdl);
            println!("opcode:{:?}", codes);
            println!("consts:{:?}", consts);
            VM::new_test(consts, codes, hdl)
        });

        println!("=========RunResult=========");
        loop {
            gc.mutate(|root: &mut VM<'_>,hdl|{
                root.step(hdl).map_err(|e|{
                    println!("{:?}",e);
                }).unwrap();
            });
            gc.collect_auto()
        }
    }
}
