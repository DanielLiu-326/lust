#[derive(Debug)]
pub struct Mod<'input> {
    pub name: Ident<'input>,
    pub stmts: Vec<Stmt<'input>>,
}

#[derive(Debug)]
pub enum Stmt<'input> {
    IfStmt {
        cond: Expr<'input>,
        success: Box<Stmt<'input>>,
        failure: Option<Box<Stmt<'input>>>,
    },
    BreakStmt,
    ContinueStmt,
    AssignStmt {
        left: LeftExpr<'input>,
        right: Expr<'input>,
    },
    VarDefineStmt {
        ident: Ident<'input>,
        right: Expr<'input>,
    },
    ConstDefineStmt {
        ident: Ident<'input>,
        right: Expr<'input>,
    },
    NormalStmt(Expr<'input>),
    WhileStmt {
        cond: Expr<'input>,
        body: Box<Stmt<'input>>,
    },
    LoopStmt(Box<Stmt<'input>>),
    BlockStmt(Vec<Stmt<'input>>),
    ReturnStmt(Expr<'input>),
    Label(Ident<'input>, Vec<Stmt<'input>>),
}

#[derive(Debug)]
pub enum LeftExpr<'input> {
    Ident(&'input str), // Register, UpValue
    Member {
        prefix: Expr<'input>,
        index: Expr<'input>,
    },
}

#[derive(Debug)]
pub enum BinaryOp {
    OpOr,
    OpAnd,
    OpBitOr,
    OpBitXor,
    OpBitAnd,
    OpNe,
    OpEq,
    OpRefEq,
    OpRefNe,
    OpLt,
    OpGt,
    OpLe,
    OpGe,
    OpBitLMov,
    OpBitRMov,
    OpAdd,
    OpSub,
    OpMult,
    OpDiv,
    OpMod,
    OpFact,
}

#[derive(Debug)]
pub enum UnaryOp {
    OpBitNot,
    OpNot,
    OpNeg,
    OpPos,
}

#[derive(Debug)]
pub enum Expr<'input> {
    Ident(Ident<'input>),
    BinaryOp {
        left: Box<Expr<'input>>,
        op: BinaryOp,
        right: Box<Expr<'input>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr<'input>>,
    },
    FnCall {
        callee: Box<Expr<'input>>,
        args: Vec<Expr<'input>>,
    },
    IndexVisit {
        expr: Box<Expr<'input>>,
        index: Box<Expr<'input>>,
    },
    Brace(Box<Expr<'input>>),
    Literal(Literal<'input>),
}

#[derive(Debug)]
pub enum Literal<'input> {
    Nil,
    Float(f64),
    Integer(i64),
    Dict(Vec<(Expr<'input>, Expr<'input>)>),
    Vector(Vec<Expr<'input>>),
    Bool(bool),
    String(String<'input>),
    Function {
        params: Vec<Ident<'input>>,
        body: Vec<Stmt<'input>>,
    },
}

pub type Ident<'input> = &'input str;
pub type String<'input> = &'input str;
