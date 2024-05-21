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
                        "struct" => Token::Struct, // 支持 struct 关键字
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
        StructDef(String, Vec<(String, String)>), // 定义结构体
        StructInstance(String, String, Vec<(String, Expr)>), // 实例化结构体))
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
                        // *pos += 1;
                        // let expr = Expr::parse_expr(tokens, pos);
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
                                if let Token::Var(ref field_type) = tokens[*pos] {
                                    fields.push((field_name.clone(), field_type.clone()));
                                    *pos += 1;
                                } else {
                                    panic!("Expected type in struct definition");
                                }
                                if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                                    *pos += 1;
                                }
                            } else {
                                panic!(
                                    "Expected field name in struct definition, {:?}",
                                    tokens[*pos]
                                );
                            }
                        }
                        if *pos >= tokens.len() || tokens[*pos] != Token::RBrace {
                            panic!("Expected '}}' in struct definition");
                        }
                        *pos += 1;
                        Stmt::StructDef(struct_name.clone(), fields)
                    } else {
                        panic!("Expected struct name");
                    }
                }
                _ => panic!(
                    "Invalid statement at token: {:?} {:?}",
                    tokens[*pos],
                    tokens[*pos - 1]
                ),
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
            let mut stmts = Vec::new();
            if *pos >= tokens.len() || tokens[*pos] != Token::LBrace {
                panic!("Expected '{{' at start of block");
            }
            *pos += 1;
            while *pos < tokens.len() && tokens[*pos] != Token::RBrace {
                if Token::NewLine == tokens[*pos] {
                    *pos += 1;
                    continue;
                }
                let stmt = Self::parse_stmt(tokens, pos);
                stmts.push(stmt);
            }
            if *pos >= tokens.len() || tokens[*pos] != Token::RBrace {
                panic!("Expected '}}' at end of block");
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
        Binary(Box<Expr>, Token, Box<Expr>),
        FieldAccess(Box<Expr>, String), // 新增的字段访问节点
        StructInstance(String, Vec<(String, Expr)>), // 新增的实例化结构体节点
    }

    impl Expr {
        pub fn parse_expr(tokens: &[Token], pos: &mut usize) -> Expr {
            Self::parse_add_sub(tokens, pos)
        }

        fn parse_add_sub(tokens: &[Token], pos: &mut usize) -> Expr {
            let mut node = Self::parse_mul_div_mod(tokens, pos);

            while *pos < tokens.len() {
                match tokens[*pos] {
                    Token::Add => {
                        *pos += 1;
                        let right = Self::parse_mul_div_mod(tokens, pos);
                        node = Expr::Binary(Box::new(node), Token::Add, Box::new(right));
                    }
                    Token::Sub => {
                        *pos += 1;
                        let right = Self::parse_mul_div_mod(tokens, pos);
                        node = Expr::Binary(Box::new(node), Token::Sub, Box::new(right));
                    }
                    _ => break,
                }
            }

            node
        }

        fn parse_mul_div_mod(tokens: &[Token], pos: &mut usize) -> Expr {
            if *pos == 95 {
                println!("95");
            }
            let mut node = Self::parse_primary(tokens, pos);

            while *pos < tokens.len() {
                match tokens[*pos] {
                    Token::Mul => {
                        *pos += 1;
                        let right = Self::parse_primary(tokens, pos);
                        node = Expr::Binary(Box::new(node), Token::Mul, Box::new(right));
                    }
                    Token::Div => {
                        *pos += 1;
                        let right = Self::parse_primary(tokens, pos);
                        node = Expr::Binary(Box::new(node), Token::Div, Box::new(right));
                    }
                    Token::Mod => {
                        *pos += 1;
                        let right = Self::parse_primary(tokens, pos);
                        node = Expr::Binary(Box::new(node), Token::Mod, Box::new(right));
                    }
                    _ => break,
                }
            }

            node
        }

        fn parse_primary(tokens: &[Token], pos: &mut usize) -> Expr {
            match tokens[*pos] {
                Token::Number(n) => {
                    *pos += 1;
                    Expr::Number(n)
                }
                Token::Var(ref var_name) => {
                    *pos += 1;
                    if *pos < tokens.len() && tokens[*pos] == Token::LBrace {
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
                        Expr::StructInstance(var_name.clone(), fields)
                    } else {
                        Expr::Var(var_name.clone())
                    }
                }
                Token::Str(ref s) => {
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
                _ => panic!("Unexpected token: {:?}", tokens[*pos]),
            }
        }

        fn parse_comparison_op(tokens: &[Token], pos: &mut usize) -> Expr {
            let mut left = Self::parse_add_sub(tokens, pos);

            while *pos < tokens.len() {
                match tokens[*pos] {
                    Token::Eq | Token::Neq | Token::Lt | Token::Gt | Token::Le | Token::Ge => {
                        let op = tokens[*pos].clone();
                        *pos += 1;
                        let right = Self::parse_add_sub(tokens, pos);
                        left = Expr::Binary(Box::new(left), op, Box::new(right));
                    }
                    _ => break,
                }
            }

            left
        }
    }
}

mod vm {
    use crate::ast::{Expr, Stmt};
    use std::{collections::HashMap, fmt::Display};

    #[derive(Debug, Clone)]
    pub enum Value {
        Number(i32),
        Str(String),
        Struct(HashMap<String, Value>), // 新增结构体类型
    }

    impl Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Value::Number(n) => write!(f, "{}", n),
                Value::Str(s) => write!(f, "{}", s),
                Value::Struct(fields) => write!(f, "{:?}", fields),
            }
        }
    }

    #[derive(Debug)]
    pub struct VM {
        variables: HashMap<String, Value>,
        structs: HashMap<String, Vec<String>>, // 存储结构体定义
    }

    impl VM {
        pub fn new() -> VM {
            VM {
                variables: HashMap::new(),
                structs: HashMap::new(),
            }
        }

        pub fn interpret(&mut self, stmts: &[Stmt]) {
            for stmt in stmts {
                self.execute_stmt(stmt);
            }
        }

        fn execute_stmt(&mut self, stmt: &Stmt) {
            match stmt {
                Stmt::Let(name, expr) => {
                    let value = self.evaluate_expr(expr);
                    self.variables.insert(name.clone(), value);
                }
                Stmt::Print(expr) => {
                    let value = self.evaluate_expr(expr);
                    println!("{}", value);
                }
                Stmt::If(expr, true_branch, false_branch) => {
                    let condition = self.evaluate_expr(expr);
                    if let Value::Number(n) = condition {
                        if n != 0 {
                            self.interpret(true_branch);
                        } else if let Some(branch) = false_branch {
                            self.interpret(branch);
                        }
                    } else {
                        panic!("Condition of if statement is not a number");
                    }
                }
                Stmt::StructDef(name, fields) => {
                    let field_names = fields.iter().map(|(f, _)| f.clone()).collect();
                    self.structs.insert(name.clone(), field_names);
                }
                Stmt::StructInstance(var_name, struct_name, fields) => {
                    if let Some(field_names) = self.structs.get(struct_name).cloned() {
                        let mut struct_values = HashMap::new();
                        for (field_name, expr) in fields {
                            if field_names.contains(field_name) {
                                let value = self.evaluate_expr(expr);
                                struct_values.insert(field_name.clone(), value);
                            } else {
                                panic!(
                                    "Undefined field: {} for struct {}",
                                    field_name, struct_name
                                );
                            }
                        }
                        self.variables
                            .insert(var_name.clone(), Value::Struct(struct_values));
                    } else {
                        panic!("Unknown struct {}", struct_name);
                    }
                }
            }
        }

        fn evaluate_expr(&mut self, expr: &Expr) -> Value {
            match expr {
                Expr::Number(n) => Value::Number(*n),
                Expr::Var(name) => {
                    if let Some(value) = self.variables.get(name) {
                        value.clone()
                    } else {
                        panic!("Undefined variable: {}", name);
                    }
                }
                Expr::Str(s) => Value::Str(s.clone()),
                Expr::Binary(left, op, right) => {
                    let left_val = self.evaluate_expr(left);
                    let right_val = self.evaluate_expr(right);
                    match (left_val, right_val) {
                        (Value::Number(l), Value::Number(r)) => match op {
                            crate::lexer::Token::Add => Value::Number(l + r),
                            crate::lexer::Token::Sub => Value::Number(l - r),
                            crate::lexer::Token::Mul => Value::Number(l * r),
                            crate::lexer::Token::Div => Value::Number(l / r),
                            crate::lexer::Token::Mod => Value::Number(l % r),
                            crate::lexer::Token::Gt => Value::Number(if l > r { 1 } else { 0 }),
                            crate::lexer::Token::Ge => Value::Number(if l >= r { 1 } else { 0 }),
                            crate::lexer::Token::Lt => Value::Number(if l < r { 1 } else { 0 }),
                            crate::lexer::Token::Le => Value::Number(if l <= r { 1 } else { 0 }),
                            crate::lexer::Token::Eq => Value::Number(if l == r { 1 } else { 0 }),
                            crate::lexer::Token::Neq => Value::Number(if l != r { 1 } else { 0 }),
                            _ => panic!("Invalid binary operator, {:?}", op),
                        },
                        _ => panic!("Invalid operands for binary operator"),
                    }
                }
                Expr::FieldAccess(expr, field) => {
                    let value = self.evaluate_expr(expr);
                    if let Value::Struct(fields) = value {
                        if let Some(field_value) = fields.get(field) {
                            field_value.clone()
                        } else {
                            panic!("Undefined field: {}", field);
                        }
                    } else {
                        panic!("Field access on non-struct value");
                    }
                }
                Expr::StructInstance(_name, fields) => {
                    let mut field_values = HashMap::new();
                    for (field_name, expr) in fields {
                        let value = self.evaluate_expr(expr);
                        field_values.insert(field_name.clone(), value);
                    }
                    Value::Struct(field_values)
                }
            }
        }
    }
}

use std::fs::File;
use std::io::Read;

use crate::ast::Stmt;
use crate::lexer::tokenize;
use crate::vm::VM;

fn main() {
    let mut source = String::new();
    let mut source_file = File::open("source.nl").unwrap();
    _ = source_file.read_to_string(&mut source);

    let tokens = tokenize(source.as_str());
    let ast = Stmt::parse_tokens(&tokens);
    let mut vm = VM::new();
    vm.interpret(&ast);
}
