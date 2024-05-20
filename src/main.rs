use std::{fs::File, io::Read};

mod lexer {
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Number(i32),
        Var(String),
        Str(String), // 新增字符串Token
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        LParen,
        RParen,
        LBrace,
        RBrace,
        Let,
        Print,
        Assign,
        If,
        Else,
        Eq,      // ==
        Neq,     // !=
        Lt,      // <
        Gt,      // >
        Le,      // <=
        Ge,      // >=
        NewLine, // 表示换行符
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
                        _ => Token::Var(ident),
                    });
                }
                '"' => {
                    // 处理字符串字面量
                    chars.next(); // skip the opening quote
                    let mut string_lit = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch != '"' {
                            string_lit.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    chars.next(); // skip the closing quote
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
        Print(Expr),                            // 修改Print语句以支持任意表达式
        If(Expr, Vec<Stmt>, Option<Vec<Stmt>>), // 条件，真分支，假分支(可选)
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
                        let expr = Expr::parse_expr(tokens, pos);
                        Stmt::Let(var_name.clone(), expr)
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
                _ => panic!("Invalid statement at token: {:?}", tokens[*pos]),
            }
        }

        fn parse_if_branches(tokens: &[Token], pos: &mut usize) -> (Vec<Stmt>, Option<Vec<Stmt>>) {
            let true_branch = Self::parse_block(tokens, pos);
            let false_branch = if *pos < tokens.len() && tokens[*pos] == Token::Else {
                *pos += 1; // skip 'else'
                if *pos < tokens.len() && tokens[*pos] == Token::If {
                    *pos += 1; // skip 'if'
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
                panic!("Expected opening brace at the start of a block");
            }
            *pos += 1; // skip '{'
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
                panic!("Expected closing brace at the end of a block");
            }
            *pos += 1; // skip '}'
            stmts
        }
    }

    #[derive(Debug)]
    pub enum CompareOp {
        Eq,
        Neq,
        Lt,
        Gt,
        Le,
        Ge,
    }

    #[derive(Debug)]
    pub enum Expr {
        Number(i32),
        Var(String),
        Str(String), // 新增字符串表达式
        Add(Box<Expr>, Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        Mul(Box<Expr>, Box<Expr>),
        Div(Box<Expr>, Box<Expr>),
        Mod(Box<Expr>, Box<Expr>),
        Compare(Box<Expr>, CompareOp, Box<Expr>), // 添加比较运算
    }

    impl Expr {
        pub fn parse_expr(tokens: &[Token], pos: &mut usize) -> Expr {
            Self::parse_add_sub(tokens, pos)
        }

        fn parse_add_sub(tokens: &[Token], pos: &mut usize) -> Expr {
            let mut expr = Self::parse_mul_div(tokens, pos);

            while *pos < tokens.len() {
                match tokens[*pos] {
                    Token::Add => {
                        *pos += 1;
                        let rhs = Self::parse_mul_div(tokens, pos);
                        expr = Expr::Add(Box::new(expr), Box::new(rhs));
                    }
                    Token::Sub => {
                        *pos += 1;
                        let rhs = Self::parse_mul_div(tokens, pos);
                        expr = Expr::Sub(Box::new(expr), Box::new(rhs));
                    }
                    _ => break,
                }
            }

            expr
        }

        fn parse_mul_div(tokens: &[Token], pos: &mut usize) -> Expr {
            let mut expr = Self::parse_factor(tokens, pos);

            while *pos < tokens.len() {
                match tokens[*pos] {
                    Token::Mul => {
                        *pos += 1;
                        let rhs = Self::parse_factor(tokens, pos);
                        expr = Expr::Mul(Box::new(expr), Box::new(rhs));
                    }
                    Token::Div => {
                        *pos += 1;
                        let rhs = Self::parse_factor(tokens, pos);
                        expr = Expr::Div(Box::new(expr), Box::new(rhs));
                    }
                    Token::Mod => {
                        *pos += 1;
                        let rhs = Self::parse_factor(tokens, pos);
                        expr = Expr::Mod(Box::new(expr), Box::new(rhs));
                    }
                    _ => break,
                }
            }

            expr
        }

        fn parse_factor(tokens: &[Token], pos: &mut usize) -> Expr {
            let token = &tokens[*pos];
            *pos += 1;

            match token {
                Token::Number(n) => Expr::Number(*n),
                Token::Var(var_name) => Expr::Var(var_name.clone()),
                Token::Str(s) => Expr::Str(s.clone()), // 处理字符串字面量
                Token::LParen => {
                    let expr = Self::parse_expr(tokens, pos);
                    if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                        panic!("Expected closing parenthesis");
                    }
                    *pos += 1; // skip ')'
                    expr
                }
                _ => panic!("Unexpected token: {:?}", token),
            }
        }

        pub fn parse_comparison_op(tokens: &[Token], pos: &mut usize) -> Expr {
            let lhs = Self::parse_expr(tokens, pos);

            let op = match tokens[*pos] {
                Token::Eq => CompareOp::Eq,
                Token::Neq => CompareOp::Neq,
                Token::Lt => CompareOp::Lt,
                Token::Gt => CompareOp::Gt,
                Token::Le => CompareOp::Le,
                Token::Ge => CompareOp::Ge,
                _ => panic!("Unexpected token in comparison: {:?}", tokens[*pos]),
            };
            *pos += 1;

            let rhs = Self::parse_expr(tokens, pos);
            Expr::Compare(Box::new(lhs), op, Box::new(rhs))
        }
    }
}

mod vm {
    use crate::ast::{CompareOp, Expr, Stmt};

    use std::collections::HashMap;

    #[derive(Debug, Clone)]
    pub enum Value {
        Number(i32),
        Str(String), // 添加字符串值
    }

    pub struct VM {
        variables: HashMap<String, Value>,
    }

    impl VM {
        pub fn new() -> Self {
            VM {
                variables: HashMap::new(),
            }
        }

        pub fn run(&mut self, stmts: &[Stmt]) {
            for stmt in stmts {
                self.run_stmt(stmt);
            }
        }

        fn run_stmt(&mut self, stmt: &Stmt) {
            match stmt {
                Stmt::Let(var_name, expr) => {
                    let value = self.eval_expr(expr);
                    self.variables.insert(var_name.clone(), value);
                }
                Stmt::Print(expr) => {
                    let value = self.eval_expr(expr);
                    match value {
                        Value::Number(n) => println!("{}", n),
                        Value::Str(s) => println!("{}", s), // 支持打印字符串
                    }
                }
                Stmt::If(cond_expr, true_branch, false_branch) => {
                    let cond_value = self.eval_expr(cond_expr);
                    let cond_bool = match cond_value {
                        Value::Number(n) => n != 0,
                        Value::Str(s) => !s.is_empty(), // 非空字符串视为真
                    };

                    if cond_bool {
                        self.run(true_branch);
                    } else if let Some(false_branch) = false_branch {
                        self.run(false_branch);
                    }
                }
            }
        }

        fn eval_expr(&self, expr: &Expr) -> Value {
            match expr {
                Expr::Number(n) => Value::Number(*n),
                Expr::Var(var_name) => self
                    .variables
                    .get(var_name)
                    .cloned()
                    .unwrap_or(Value::Number(0)),
                Expr::Str(s) => Value::Str(s.clone()), // 处理字符串表达式
                Expr::Add(lhs, rhs) => {
                    let lhs_val = self.eval_expr(lhs);
                    let rhs_val = self.eval_expr(rhs);
                    match (lhs_val, rhs_val) {
                        (Value::Number(lhs_num), Value::Number(rhs_num)) => {
                            Value::Number(lhs_num + rhs_num)
                        }
                        _ => panic!("Unsupported operand types for +"),
                    }
                }
                Expr::Sub(lhs, rhs) => {
                    let lhs_val = self.eval_expr(lhs);
                    let rhs_val = self.eval_expr(rhs);
                    match (lhs_val, rhs_val) {
                        (Value::Number(lhs_num), Value::Number(rhs_num)) => {
                            Value::Number(lhs_num - rhs_num)
                        }
                        _ => panic!("Unsupported operand types for -"),
                    }
                }
                Expr::Mul(lhs, rhs) => {
                    let lhs_val = self.eval_expr(lhs);
                    let rhs_val = self.eval_expr(rhs);
                    match (lhs_val, rhs_val) {
                        (Value::Number(lhs_num), Value::Number(rhs_num)) => {
                            Value::Number(lhs_num * rhs_num)
                        }
                        _ => panic!("Unsupported operand types for *"),
                    }
                }
                Expr::Div(lhs, rhs) => {
                    let lhs_val = self.eval_expr(lhs);
                    let rhs_val = self.eval_expr(rhs);
                    match (lhs_val, rhs_val) {
                        (Value::Number(lhs_num), Value::Number(rhs_num)) => {
                            Value::Number(lhs_num / rhs_num)
                        }
                        _ => panic!("Unsupported operand types for /"),
                    }
                }
                Expr::Mod(lhs, rhs) => {
                    let lhs_val = self.eval_expr(lhs);
                    let rhs_val = self.eval_expr(rhs);
                    match (lhs_val, rhs_val) {
                        (Value::Number(lhs_num), Value::Number(rhs_num)) => {
                            Value::Number(lhs_num % rhs_num)
                        }
                        _ => panic!("Unsupported operand types for %"),
                    }
                }
                Expr::Compare(lhs, op, rhs) => {
                    let lhs_val = self.eval_expr(lhs);
                    let rhs_val = self.eval_expr(rhs);
                    match (lhs_val, rhs_val) {
                        (Value::Number(lhs_num), Value::Number(rhs_num)) => {
                            let result = match op {
                                CompareOp::Eq => lhs_num == rhs_num,
                                CompareOp::Neq => lhs_num != rhs_num,
                                CompareOp::Lt => lhs_num < rhs_num,
                                CompareOp::Gt => lhs_num > rhs_num,
                                CompareOp::Le => lhs_num <= rhs_num,
                                CompareOp::Ge => lhs_num >= rhs_num,
                            };
                            Value::Number(result as i32)
                        }
                        _ => panic!("Unsupported operand types for comparison"),
                    }
                }
            }
        }
    }
}

use ast::Stmt;
use lexer::tokenize;
use vm::VM;

fn main() {
    let mut source = String::new();
    let mut source_file = File::open("source.nl").unwrap();
    _ = source_file.read_to_string(&mut source);

    let tokens = tokenize(source.as_str());
    let ast = Stmt::parse_tokens(&tokens);
    let mut vm = VM::new();
    vm.run(&ast);
}
