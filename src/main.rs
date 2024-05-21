mod lexer {
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Number(i32),
        Var(String),
        Str(String),
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        LParen,
        RParen,
        LBrace,
        RBrace,
        Colon, // 新增的冒号 Token
        Comma, // 新增的逗号 Token
        Dot,   // 新增的点 Token
        Let,
        Print,
        Assign,
        If,
        Else,
        Struct, // 新增的结构体 Token
        Eq,
        Neq,
        Lt,
        Gt,
        Le,
        Ge,
        NewLine,
    }

    pub fn tokenize(input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                '0'..='9' => {
                    let mut number = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_numeric() {
                            number.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(Token::Number(number.parse().unwrap()));
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut ident = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() {
                            ident.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(match ident.as_str() {
                        "let" => Token::Let,
                        "print" => Token::Print,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "struct" => Token::Struct,
                        _ => Token::Var(ident),
                    });
                }
                '"' => {
                    chars.next();
                    let mut string_lit = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch != '"' {
                            string_lit.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    chars.next();
                    tokens.push(Token::Str(string_lit));
                }
                '+' => {
                    chars.next();
                    tokens.push(Token::Add);
                }
                '-' => {
                    chars.next();
                    tokens.push(Token::Sub);
                }
                '*' => {
                    chars.next();
                    tokens.push(Token::Mul);
                }
                '/' => {
                    chars.next();
                    tokens.push(Token::Div);
                }
                '%' => {
                    chars.next();
                    tokens.push(Token::Mod);
                }
                '(' => {
                    chars.next();
                    tokens.push(Token::LParen);
                }
                ')' => {
                    chars.next();
                    tokens.push(Token::RParen);
                }
                '{' => {
                    chars.next();
                    tokens.push(Token::LBrace);
                }
                '}' => {
                    chars.next();
                    tokens.push(Token::RBrace);
                }
                ':' => {
                    chars.next();
                    tokens.push(Token::Colon);
                }
                ',' => {
                    chars.next();
                    tokens.push(Token::Comma);
                }
                '.' => {
                    chars.next();
                    tokens.push(Token::Dot);
                }
                '=' => {
                    chars.next();
                    if let Some(&'=') = chars.peek() {
                        chars.next();
                        tokens.push(Token::Eq);
                    } else {
                        tokens.push(Token::Assign);
                    }
                }
                ' ' | '\t' => {
                    chars.next();
                }
                '\n' => {
                    chars.next();
                    tokens.push(Token::NewLine);
                }
                '\r' => {
                    chars.next();
                    if let Some(&'\n') = chars.peek() {
                        chars.next();
                    }
                    tokens.push(Token::NewLine);
                }
                '!' => {
                    chars.next();
                    if let Some(&'=') = chars.peek() {
                        chars.next();
                        tokens.push(Token::Neq);
                    } else {
                        panic!("Unexpected character after '!': expected '='");
                    }
                }
                '<' => {
                    chars.next();
                    if let Some(&'=') = chars.peek() {
                        chars.next();
                        tokens.push(Token::Le);
                    } else {
                        tokens.push(Token::Lt);
                    }
                }
                '>' => {
                    chars.next();
                    if let Some(&'=') = chars.peek() {
                        chars.next();
                        tokens.push(Token::Ge);
                    } else {
                        tokens.push(Token::Gt);
                    }
                }
                _ => {
                    panic!("Unexpected character: {}", ch);
                }
            }
        }

        tokens
    }
}

mod ast {
    use crate::lexer::Token;

    #[derive(Debug)]
    pub enum Stmt {
        Let(String, Expr),
        Print(Expr),
        If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
        StructDef(String, Vec<(String, String)>),
        StructInstance(String, String, Vec<(String, Expr)>),
    }

    impl Stmt {
        pub fn parse_tokens(tokens: &[Token]) -> Vec<Stmt> {
            let mut pos = 0;
            let mut stmts = Vec::new();

            while pos < tokens.len() {
                if tokens[pos] == Token::NewLine {
                    pos += 1;
                    continue;
                }
                let stmt = Self::parse_stmt(tokens, &mut pos);
                stmts.push(stmt);
            }

            stmts
        }

        fn parse_stmt(tokens: &[Token], pos: &mut usize) -> Stmt {
            match tokens[*pos] {
                Token::Let => {
                    *pos += 1;
                    if *pos >= tokens.len() {
                        panic!("Unexpected end of tokens in let statement");
                    }
                    if let Token::Var(ref var_name) = tokens[*pos] {
                        *pos += 1;
                        if *pos >= tokens.len() || tokens[*pos] != Token::Assign {
                            panic!("Expected '=' in let statement");
                        }
                        *pos += 1;
                        if *pos < tokens.len() {
                            match &tokens[*pos] {
                                Token::Var(struct_name)
                                    if *pos + 1 < tokens.len()
                                        && tokens[*pos + 1] == Token::LBrace =>
                                {
                                    *pos += 2;
                                    let mut fields = Vec::new();
                                    while *pos < tokens.len() && tokens[*pos] != Token::RBrace {
                                        if Token::NewLine == tokens[*pos] {
                                            *pos += 1;
                                            continue;
                                        }
                                        if let Token::Var(ref field_name) = tokens[*pos] {
                                            *pos += 1;
                                            if *pos >= tokens.len() || tokens[*pos] != Token::Colon
                                            {
                                                panic!("Expected ':' in struct instance");
                                            }
                                            *pos += 1;
                                            let expr = Expr::parse_expr(tokens, pos);
                                            fields.push((field_name.clone(), expr));
                                            if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                                                *pos += 1;
                                            }
                                        } else {
                                            panic!(
                                                "Expected field name in struct instance, {:?}",
                                                tokens[*pos]
                                            );
                                        }
                                    }
                                    if *pos >= tokens.len() || tokens[*pos] != Token::RBrace {
                                        panic!("Expected '}}' in struct instance");
                                    }
                                    *pos += 1;
                                    return Stmt::StructInstance(
                                        var_name.clone(),
                                        struct_name.clone(),
                                        fields,
                                    );
                                }
                                _ => {
                                    let expr = Expr::parse_expr(tokens, pos);
                                    return Stmt::Let(var_name.clone(), expr);
                                }
                            }
                        }
                        panic!("Expected expression in let statement")
                    } else {
                        panic!("Expected variable name in let statement");
                    }
                }
                Token::Print => {
                    *pos += 1;
                    let expr = Expr::parse_expr(tokens, pos);
                    Stmt::Print(expr)
                }
                Token::If => {
                    *pos += 1;
                    let expr = Expr::parse_comparison_op(tokens, pos);
                    let (true_branch, false_branch) = Self::parse_if_branches(tokens, pos);
                    Stmt::If(expr, true_branch, false_branch)
                }
                Token::Struct => {
                    *pos += 1;
                    if let Token::Var(ref struct_name) = tokens[*pos] {
                        *pos += 1;
                        if *pos >= tokens.len() || tokens[*pos] != Token::LBrace {
                            panic!("Expected '{{' in struct definition");
                        }
                        *pos += 1;
                        let mut fields = Vec::new();
                        while *pos < tokens.len() && tokens[*pos] != Token::RBrace {
                            if Token::NewLine == tokens[*pos] {
                                *pos += 1;
                                continue;
                            }
                            if let Token::Var(ref field_name) = tokens[*pos] {
                                *pos += 1;
                                if *pos >= tokens.len() || tokens[*pos] != Token::Colon {
                                    panic!("Expected ':' in struct definition");
                                }
                                *pos += 1;
                                if *pos >= tokens.len() {
                                    panic!("Unexpected end of tokens in struct definition");
                                }
                                if let Token::Var(ref field_type) = tokens[*pos] {
                                    *pos += 1;
                                    fields.push((field_name.clone(), field_type.clone()));
                                    if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                                        *pos += 1;
                                    }
                                } else {
                                    panic!("Expected type name in struct definition");
                                }
                            } else {
                                panic!("Expected field name in struct definition");
                            }
                        }
                        if *pos >= tokens.len() || tokens[*pos] != Token::RBrace {
                            panic!("Expected '}}' in struct definition");
                        }
                        *pos += 1;
                        return Stmt::StructDef(struct_name.clone(), fields);
                    } else {
                        panic!("Expected struct name in struct definition");
                    }
                }
                _ => panic!("Unexpected token in statement: {:?}", tokens[*pos]),
            }
        }

        fn parse_if_branches(tokens: &[Token], pos: &mut usize) -> (Vec<Stmt>, Option<Vec<Stmt>>) {
            let true_branch = Self::parse_block(tokens, pos);
            let false_branch = if *pos < tokens.len() && tokens[*pos] == Token::Else {
                *pos += 1;
                if *pos < tokens.len() && tokens[*pos] == Token::If {
                    *pos += 1;
                    let expr = Expr::parse_comparison_op(tokens, pos);
                    let (elif_true_branch, elif_false_branch) =
                        Self::parse_if_branches(tokens, pos);
                    let elif_stmt = Stmt::If(expr, elif_true_branch, elif_false_branch);
                    Some(vec![elif_stmt])
                } else {
                    Some(Self::parse_block(tokens, pos))
                }
            } else {
                None
            };
            (true_branch, false_branch)
        }

        fn parse_block(tokens: &[Token], pos: &mut usize) -> Vec<Stmt> {
            if *pos >= tokens.len() || tokens[*pos] != Token::LBrace {
                panic!("Expected '{{' to start block");
            }
            *pos += 1;
            let mut stmts = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != Token::RBrace {
                if tokens[*pos] == Token::NewLine {
                    *pos += 1;
                    continue;
                }
                let stmt = Self::parse_stmt(tokens, pos);
                stmts.push(stmt);
            }
            if *pos >= tokens.len() || tokens[*pos] != Token::RBrace {
                panic!("Expected '}}' to end block");
            }
            *pos += 1;
            stmts
        }
    }

    #[derive(Debug)]
    pub enum Expr {
        Number(i32),
        Var(String),
        Str(String),
        BinOp(Box<Expr>, BinOp, Box<Expr>),
        StructField(Box<Expr>, String), // 结构体字段
    }

    impl Expr {
        fn parse_expr(tokens: &[Token], pos: &mut usize) -> Expr {
            Self::parse_add_sub(tokens, pos)
        }

        fn parse_add_sub(tokens: &[Token], pos: &mut usize) -> Expr {
            let mut lhs = Self::parse_mul_div_mod(tokens, pos);
            while *pos < tokens.len() {
                match tokens[*pos] {
                    Token::Add => {
                        *pos += 1;
                        let rhs = Self::parse_mul_div_mod(tokens, pos);
                        lhs = Expr::BinOp(Box::new(lhs), BinOp::Add, Box::new(rhs));
                    }
                    Token::Sub => {
                        *pos += 1;
                        let rhs = Self::parse_mul_div_mod(tokens, pos);
                        lhs = Expr::BinOp(Box::new(lhs), BinOp::Sub, Box::new(rhs));
                    }
                    _ => break,
                }
            }
            lhs
        }

        fn parse_mul_div_mod(tokens: &[Token], pos: &mut usize) -> Expr {
            let mut lhs = Self::parse_primary(tokens, pos);
            while *pos < tokens.len() {
                match tokens[*pos] {
                    Token::Mul => {
                        *pos += 1;
                        let rhs = Self::parse_primary(tokens, pos);
                        lhs = Expr::BinOp(Box::new(lhs), BinOp::Mul, Box::new(rhs));
                    }
                    Token::Div => {
                        *pos += 1;
                        let rhs = Self::parse_primary(tokens, pos);
                        lhs = Expr::BinOp(Box::new(lhs), BinOp::Div, Box::new(rhs));
                    }
                    Token::Mod => {
                        *pos += 1;
                        let rhs = Self::parse_primary(tokens, pos);
                        lhs = Expr::BinOp(Box::new(lhs), BinOp::Mod, Box::new(rhs));
                    }
                    _ => break,
                }
            }
            lhs
        }

        fn parse_primary(tokens: &[Token], pos: &mut usize) -> Expr {
            if *pos >= tokens.len() {
                panic!("Unexpected end of tokens in expression");
            }
            match &tokens[*pos] {
                Token::Number(n) => {
                    *pos += 1;
                    Expr::Number(*n)
                }
                Token::Var(var_name) => {
                    *pos += 1;
                    if *pos < tokens.len() && tokens[*pos] == Token::Dot {
                        *pos += 1;
                        if let Token::Var(field_name) = &tokens[*pos] {
                            *pos += 1;
                            return Expr::StructField(
                                Box::new(Expr::Var(var_name.clone())),
                                field_name.clone(),
                            );
                        } else {
                            panic!("Expected field name after '.' in struct field access");
                        }
                    }
                    Expr::Var(var_name.clone())
                }
                Token::Str(s) => {
                    *pos += 1;
                    Expr::Str(s.clone())
                }
                Token::LParen => {
                    *pos += 1;
                    let expr = Self::parse_expr(tokens, pos);
                    if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                        panic!("Expected ')' after expression");
                    }
                    *pos += 1;
                    expr
                }
                _ => panic!("Unexpected token in primary expression: {:?}", tokens[*pos]),
            }
        }

        fn parse_comparison_op(tokens: &[Token], pos: &mut usize) -> Expr {
            let lhs = Self::parse_add_sub(tokens, pos);
            if *pos < tokens.len() {
                let op = match tokens[*pos] {
                    Token::Eq => BinOp::Eq,
                    Token::Neq => BinOp::Neq,
                    Token::Lt => BinOp::Lt,
                    Token::Gt => BinOp::Gt,
                    Token::Le => BinOp::Le,
                    Token::Ge => BinOp::Ge,
                    _ => return lhs,
                };
                *pos += 1;
                let rhs = Self::parse_add_sub(tokens, pos);
                Expr::BinOp(Box::new(lhs), op, Box::new(rhs))
            } else {
                lhs
            }
        }
    }

    #[derive(Debug)]
    pub enum BinOp {
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        Eq,
        Neq,
        Lt,
        Gt,
        Le,
        Ge,
    }
}

mod vm {
    use crate::ast::{BinOp, Expr, Stmt};
    use std::collections::HashMap;

    pub struct VM {
        variables: HashMap<String, Value>,
        structs: HashMap<String, StructDef>,
    }

    #[derive(Debug, Clone)]
    pub enum Value {
        Number(i32),
        Str(String),
        StructInstance(HashMap<String, Value>),
    }

    #[derive(Debug, Clone)]
    pub struct StructDef {
        fields: HashMap<String, String>,
    }

    impl VM {
        pub fn new() -> Self {
            VM {
                variables: HashMap::new(),
                structs: HashMap::new(),
            }
        }

        pub fn run(&mut self, stmts: &[Stmt]) {
            for stmt in stmts {
                self.execute_stmt(stmt);
            }
        }

        fn execute_stmt(&mut self, stmt: &Stmt) {
            match stmt {
                Stmt::Let(var_name, expr) => {
                    let value = self.evaluate_expr(expr);
                    self.variables.insert(var_name.clone(), value);
                }
                Stmt::Print(expr) => {
                    let value = self.evaluate_expr(expr);
                    println!("{:?}", value);
                }
                Stmt::If(cond, true_branch, false_branch) => {
                    if self.evaluate_cond(cond) {
                        self.run(true_branch);
                    } else if let Some(false_branch) = false_branch {
                        self.run(false_branch);
                    }
                }
                Stmt::StructDef(name, fields) => {
                    let mut field_map = HashMap::new();
                    for (field_name, field_type) in fields {
                        field_map.insert(field_name.clone(), field_type.clone());
                    }
                    self.structs
                        .insert(name.clone(), StructDef { fields: field_map });
                }
                Stmt::StructInstance(var_name, struct_name, fields) => {
                    if let Some(struct_def) = self.structs.get(struct_name) {
                        let mut instance = HashMap::new();
                        for (field_name, expr) in fields {
                            let value = self.evaluate_expr(expr);
                            if let Some(_field_type) = struct_def.fields.get(field_name) {
                                instance.insert(field_name.clone(), value);
                            } else {
                                panic!(
                                    "Field '{}' not found in struct '{}'",
                                    field_name, struct_name
                                );
                            }
                        }
                        self.variables
                            .insert(var_name.clone(), Value::StructInstance(instance));
                    } else {
                        panic!("Struct '{}' not defined", struct_name);
                    }
                }
            }
        }

        fn evaluate_expr(&self, expr: &Expr) -> Value {
            match expr {
                Expr::Number(n) => Value::Number(*n),
                Expr::Var(var_name) => {
                    if let Some(value) = self.variables.get(var_name) {
                        value.clone()
                    } else {
                        panic!("Variable '{}' not found", var_name);
                    }
                }
                Expr::Str(s) => Value::Str(s.clone()),
                Expr::BinOp(lhs, op, rhs) => {
                    let lhs_val = self.evaluate_expr(lhs);
                    let rhs_val = self.evaluate_expr(rhs);
                    match (lhs_val, rhs_val, op) {
                        (Value::Number(l), Value::Number(r), BinOp::Add) => Value::Number(l + r),
                        (Value::Number(l), Value::Number(r), BinOp::Sub) => Value::Number(l - r),
                        (Value::Number(l), Value::Number(r), BinOp::Mul) => Value::Number(l * r),
                        (Value::Number(l), Value::Number(r), BinOp::Div) => Value::Number(l / r),
                        (Value::Number(l), Value::Number(r), BinOp::Mod) => Value::Number(l % r),
                        (Value::Number(l), Value::Number(r), BinOp::Eq) => {
                            Value::Number((l == r) as i32)
                        }
                        (Value::Number(l), Value::Number(r), BinOp::Neq) => {
                            Value::Number((l != r) as i32)
                        }
                        (Value::Number(l), Value::Number(r), BinOp::Lt) => {
                            Value::Number((l < r) as i32)
                        }
                        (Value::Number(l), Value::Number(r), BinOp::Gt) => {
                            Value::Number((l > r) as i32)
                        }
                        (Value::Number(l), Value::Number(r), BinOp::Le) => {
                            Value::Number((l <= r) as i32)
                        }
                        (Value::Number(l), Value::Number(r), BinOp::Ge) => {
                            Value::Number((l >= r) as i32)
                        }
                        _ => panic!("Invalid binary operation"),
                    }
                }
                Expr::StructField(expr, field_name) => {
                    if let Value::StructInstance(instance) = self.evaluate_expr(expr) {
                        if let Some(value) = instance.get(field_name) {
                            value.clone()
                        } else {
                            panic!("Field '{}' not found in struct instance", field_name);
                        }
                    } else {
                        panic!("Expected struct instance in field access");
                    }
                }
            }
        }

        fn evaluate_cond(&self, expr: &Expr) -> bool {
            match self.evaluate_expr(expr) {
                Value::Number(n) => n != 0,
                _ => panic!("Expected numeric value in condition"),
            }
        }
    }
}

use std::fs::File;
use std::io::Read;

fn main() {
    let mut source = String::new();
    let mut source_file = File::open("source.nl").unwrap();
    _ = source_file.read_to_string(&mut source);

    let tokens = lexer::tokenize(&mut source);
    // println!("{:?}", tokens);

    let ast = ast::Stmt::parse_tokens(&tokens);
    // println!("{:?}", ast);

    let mut vm = vm::VM::new();
    vm.run(&ast);
}
