use crate::ast;
pub use my_parser::*;
use std::str::FromStr;

peg::parser! { grammar my_parser() for str{
    rule _ = [' ' | '\n']*
    rule blank() = [' ' | '\n']+
    //终结符定义
    rule OP_BIT_NOT()   =  "~" _
    rule OP_BIT_LMOV()  =  "<<" _
    rule OP_BIT_RMOV()  =  ">>" _
    rule OP_BIT_AND()   =  "&" _
    rule OP_BIT_OR()    =  "|" _
    rule OP_BIT_XOR()   =  "^" _

    rule OP_ADD()       =  "+" _
    rule OP_SUB()       =  "-" _
    rule OP_MULT()      =  "*" _
    rule OP_DIV()       =  "/" _
    rule OP_MOD()       =  "%" _
    rule OP_FACT()      =  "**" _

    rule OP_EQ()        =  "==" _
    rule OP_NE()        =  "!=" _
    rule OP_GT()        =  ">" _
    rule OP_LT()        =  "<" _
    rule OP_GE()        =  ">=" _
    rule OP_LE()        =  "<=" _

    rule OP_REF_EQ()    =  ":==" _
    rule OP_REF_NE()    =  ":!=" _
    // rule OP_REF_GT()    =  ":>" _
    // rule OP_REF_LT()    =  ":<" _
    // rule OP_REF_GE()    =  ":>=" _
    // rule OP_REF_LE()    =  ":<=" _

    rule OP_AND()       =  "&&" _
    rule OP_OR()        =  "||" _
    rule OP_NOT()       =  "!" _

    rule OP_ASSIGN()    =  "=" _

    // rule OP_REF_ASSIGN()=  ":=" _

    rule OP_ARROW_RIGHT()   ="->" _

    // 整型常量
    rule VAL_INTEGER_HEX()->i64 =  "0x" n:$((['0'..='9']/['a'..='f']/['A'..='F'])+) _{?
        i64::from_str_radix(n,16).map_err(|e|{
            "error occured during parsing integer literal"
        })
    }

    rule VAL_INTEGER_OCT()->i64 =  "0" n:$(['0'..='9']+) _{?
        i64::from_str_radix(n,8).map_err(|e|{
            "error occured during parsing integer literal"
        })
    }

    rule VAL_INTEGER_DEC()->i64 =  n:$(['0'..='9']+) _{?
        i64::from_str_radix(n,10).map_err(|e|{
            "error occured during parsing integer literal"
        })
    }

    rule VAL_INTEGER()->i64 =  n:(VAL_INTEGER_HEX()/VAL_INTEGER_OCT()/VAL_INTEGER_DEC()) _{
        n
    }

    // 浮点常量
    rule VAL_FLOAT()->f64   =  n:$(['0'..='9']+ ("." ['0'..='9']*)) _{
        f64::from_str(n).unwrap()
    }

    // FIXME:字符串常量BUG
    rule VAL_STR()->&'input str =  n:$("\" "[^ '\n']* "\"") _{
        n
    }

    // 布尔常量
    rule VAL_BOOL()->bool =  n:$("true"/"false") _ {
        n=="true"
    }

    // Nil常量
    rule VAL_NIL() =  "nil" _

    // 关键字
    rule KW_IF()   =  "if" _
    rule KW_ELSE() =  "else" _
    rule KW_WHILE()=  "while" _
    rule KW_BREAK()=  "break" _
    rule KW_CONTINUE()  =  "continue" _
    rule KW_IMPORT()    =  "import" _
    rule KW_RETURN()    =  "return" _
    rule KW_SELF()      =  "self" _
    rule KW_LOOP()      =  "loop" _
    rule KW_DICT()    = "dict" _
    rule KW_FN()        = "fn" _
    rule KW_CONST()     = "const" _
    //rule KW_VAR()       = "var" _
    rule KW_LET()       = "let" _
    rule KW_MOD()       = "mod" _
    rule KW_PUB()       = "pub" _
    rule KW_USE()       = "use" _

    rule BRACE_S_L()    = "(" _
    rule BRACE_S_R()    = ")" _
    rule BRACE_M_L()    = "[" _
    rule BRACE_M_R()    = "]" _
    rule BRACE_L_L()    = "{" _
    rule BRACE_L_R()    = "}" _

    rule COMMA()        = "," _
    rule SEMICOLON()    = ";" _
    rule DOT()          = "." _
    rule COLON()        = ":" _
    rule DOUBLE_COLON()  = "::" _
    rule IDENT()->&'input str  = !KEY_WORDS() n:$(['a'..='z'|'_'|'A'..='Z']['a'..='z'|'A'..='Z'|'_'|'0'..='9']*) _{
        n
    }

    rule KEY_WORDS()
        = KW_IF()/KW_ELSE()/KW_WHILE()/
        KW_BREAK()/KW_CONTINUE()/KW_IMPORT()/
        KW_RETURN()/KW_SELF()/KW_LOOP()/
        KW_DICT()/KW_FN()/KW_CONST()/
        KW_LET()/KW_MOD()/KW_PUB()/
        KW_USE()/VAL_NIL()/VAL_BOOL()

    pub rule src_file() -> ast::Mod<'input >
        = mod_body:mod_body(){
        ast::Mod{
            //TODO
            name:"todo",
            stmts:mod_body
        }
    }

    rule mod_body() -> Vec<ast::Stmt<'input>>
        = stmts:stmts(){
        stmts
    }
    // 语句stmt定义
    rule stmts() -> Vec<ast::Stmt<'input>>
        = stmts:stmt_or_blank()* {
        let mut ret = Vec::new();
        for x in stmts{
            if let Some(stmt) = x {
                ret.push(stmt);
            }
        }
        ret
    }

    rule stmt_or_blank() -> Option<ast::Stmt<'input>>
        = stmt:stmt(){
        Some(stmt)
    } / blank() {
        None
    }

    rule stmt()->ast::Stmt<'input>
        = n:(if_stmt()/break_stmt()/continue_stmt()/
        assign_stmt()/var_define_stmt()/const_define_stmt()/
        normal_stmt()/while_stmt()/loop_stmt()/
        block_stmt()/return_stmt())
    {
        n
    }

    rule if_stmt()->ast::Stmt<'input>
        = precedence!{
        KW_IF() BRACE_S_L() cond:expr() BRACE_S_R() success:stmt() KW_ELSE() failure:stmt() {
            ast::Stmt::IfStmt{
                cond,
                success:Box::new(success),
                failure:Some(Box::new(failure)),
            }
        }
        --
        KW_IF() BRACE_S_L() cond:expr() BRACE_S_R() success:stmt(){
            ast::Stmt::IfStmt{
                cond,
                success:Box::new(success),
                failure:None
            }
        }
    }

    rule break_stmt()->ast::Stmt<'input>
        =KW_BREAK() SEMICOLON() {
        ast::Stmt::BreakStmt
    }

    rule continue_stmt()->ast::Stmt<'input>
        =KW_CONTINUE() SEMICOLON() {
        ast::Stmt::ContinueStmt
    }

    rule assign_stmt() -> ast::Stmt<'input>
        = left:left_expr() OP_ASSIGN() right:expr() SEMICOLON(){
        ast::Stmt::AssignStmt{
            left,
            right,
        }.into()
    }

    rule var_define_stmt() -> ast::Stmt<'input>
        = KW_LET() ident:IDENT() OP_ASSIGN() right:expr() SEMICOLON() {
        ast::Stmt::VarDefineStmt{
            ident,
            right,
        }
    }

    rule const_define_stmt() -> ast::Stmt<'input>
        = KW_CONST() ident:IDENT() OP_ASSIGN() right:expr() SEMICOLON() {
        ast::Stmt::ConstDefineStmt{
            ident,
            right,
        }
    }

    rule normal_stmt()->ast::Stmt<'input>
        = expr:expr() SEMICOLON(){
        ast::Stmt::NormalStmt(expr)
    }

    rule while_stmt()->ast::Stmt<'input>
        = KW_WHILE() BRACE_S_L() cond:expr() BRACE_S_R()  body:stmt(){
        ast::Stmt::WhileStmt{
            cond,
            body:Box::new(body),
        }
    }

    rule loop_stmt()->ast::Stmt<'input>
        = KW_LOOP() body:stmt(){
        ast::Stmt::LoopStmt(Box::new(body))
    }

    rule block_stmt()->ast::Stmt<'input>
        = BRACE_L_L() stmts:stmts() BRACE_L_R(){
        ast::Stmt::BlockStmt(stmts)
    }

    rule return_stmt()->ast::Stmt<'input>
        = KW_RETURN() expr:expr() SEMICOLON(){
        ast::Stmt::ReturnStmt(expr)
    }

    rule left_expr()->ast::LeftExpr<'input>
        = prefix:expr() BRACE_M_L() index:expr() BRACE_M_R()
    {
        ast::LeftExpr::Member{
            prefix,
            index
        }
    } / i:IDENT() {
        ast::LeftExpr::Ident(i)
    }

    rule expr()->ast::Expr<'input>
        = precedence!{
        i:IDENT() {
            ast::Expr::Ident(i)
        }
        --
        left:(@) OP_OR() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpOr,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_AND() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpAnd,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_BIT_OR() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpBitOr,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_BIT_XOR() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpBitXor,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_BIT_AND() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpBitAnd,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_NE() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpNe,
                right:Box::new(right),
            }
        }
        left:(@) OP_EQ() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpEq,
                right:Box::new(right),
            }
        }
        left:(@) OP_REF_EQ() right:@{
            todo!()
        }
        left:(@) OP_REF_NE() right:@{
            todo!()
        }
        --
        left:(@) OP_LT() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpLt,
                right:Box::new(right),
            }
        }

        left:(@) OP_GT() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpGt,
                right:Box::new(right),
            }
        }

        left:(@) OP_LE() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpLe,
                right:Box::new(right),
            }
        }

        left:(@) OP_GE() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpGe,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_BIT_LMOV() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpBitLMov,
                right:Box::new(right),
            }
        }

        left:(@) OP_BIT_RMOV() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpBitRMov,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_ADD() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpAdd,
                right:Box::new(right),
            }
        }

        left:(@) OP_SUB() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpSub,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_MULT() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpMult,
                right:Box::new(right),
            }
        }

        left:(@) OP_DIV() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpDiv,
                right:Box::new(right),
            }
        }

        left:(@) OP_MOD() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpMod,
                right:Box::new(right),
            }
        }
        --
        left:(@) OP_FACT() right:@{
            ast::Expr::BinaryOp{
                left:Box::new(left),
                op:ast::BinaryOp::OpFact,
                right:Box::new(right),
            }
        }
        --
        //unary
        OP_BIT_NOT() expr:(@){
            ast::Expr::UnaryOp{
                op:ast::UnaryOp::OpNot,
                expr:Box::new(expr),
            }
        }
        OP_NOT() expr:(@){
            ast::Expr::UnaryOp{
                op:ast::UnaryOp::OpNot,
                expr:Box::new(expr),
            }
        }
        OP_SUB() expr:(@){
            ast::Expr::UnaryOp{
                op:ast::UnaryOp::OpNeg,
                expr:Box::new(expr),
            }
        }
        OP_ADD() expr:(@){
            ast::Expr::UnaryOp{
                op:ast::UnaryOp::OpPos,
                expr:Box::new(expr),
            }
        }
        //函数调用
        callee:@ BRACE_S_L() args:arg_list() BRACE_S_R(){
            ast::Expr::FnCall{
                callee:Box::new(callee),
                args
            }
        }
        //索引访问
        expr:@ BRACE_M_L() index:expr() BRACE_M_R(){
            ast::Expr::IndexVisit{
                expr:Box::new(expr),
                index:Box::new(index),
            }
        }
        --
        //成员访问运算符
        left:@ DOT() ident:IDENT(){
            todo!()
        }
        --
        BRACE_S_L() expr:expr() BRACE_S_R(){
            ast::Expr::Brace(Box::new(expr))
        }
        --
        //各种值
        n:literal(){
            ast::Expr::Literal(n)
        }
    }

    rule literal()->ast::Literal<'input>
        = n:(val_float()/val_integer()/val_dict()/val_nil()/val_bool()/val_str()/val_function()){
        n
    }

    rule val_str()->ast::Literal<'input>
        = val:VAL_STR() {
        ast::Literal::String(val)
    }

    rule val_bool()->ast::Literal<'input>
        = val:VAL_BOOL(){
        ast::Literal::Bool(val)
    }

    rule val_nil()->ast::Literal<'input>
        = VAL_NIL(){
        ast::Literal::Nil
    }

    rule val_integer()->ast::Literal<'input>
        = value:VAL_INTEGER(){
        ast::Literal::Integer(value)
    }

    rule val_float()->ast::Literal<'input>
        = value:VAL_FLOAT(){
        ast::Literal::Float(value)
    }

    rule val_dict()->ast::Literal<'input>
        = KW_DICT() BRACE_L_L() fields:dict_fields() BRACE_L_R(){
        ast::Literal::Dict(fields)
    }

    rule dict_fields() -> Vec<(ast::Expr<'input>,ast::Expr<'input>)>
        = fields:dict_field() ** COMMA() COMMA()?{
        fields
    }

    rule dict_field()->(ast::Expr<'input>,ast::Expr<'input>)
        = key:expr() COLON() val:expr(){
        (key,val)
    }

    rule val_function() -> ast::Literal<'input>
        = KW_FN() BRACE_S_L() params:param_list() BRACE_S_R() BRACE_L_L() body:stmts() BRACE_L_R() {
        ast::Literal::Function{
            params,
            body
        }
    }

    rule arg_list() -> Vec<ast::Expr<'input>>
        = args:expr() ** COMMA(){
        args
    }

    rule param_list() -> Vec<ast::Ident<'input >>
        = params:IDENT() ** COMMA() {
        params
    }

}}

#[cfg(test)]
mod tests {
    use crate::compiler::parser::my_parser;

    #[test]
    fn test() {
        println!(
            "{:#?}",
            my_parser::src_file(
                r#"
        let pi = 3.141592654;
        let circle = fn (r) {
            let pi = 3.141592653;
            let l  = 2*pi*r;
            let s  = pi*r**2;
            return dict{
                l:l,
                r:r,
            };
        };

        let r      = input();
        let result = circle(r);

        print(result);
        "#
            )
            .unwrap()
        );
    }
}
