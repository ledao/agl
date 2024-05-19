use std::io;

mod lexer {
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Number(i32),
        Var(String),
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
        EndIf,
        Eq,  // ==
        Neq, // !=
        Lt,  // <
        Gt,  // >
        Le,  // <=
        Ge,  // >=
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
        Print(String),
        If(Expr, Vec<Stmt>, Option<Vec<Stmt>>), // 条件，真分支，假分支(可选)
    }

    impl Stmt {
        pub fn parse_file(tokens: &[Token]) -> Vec<Stmt> {
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
                    if *pos >= tokens.len() {
                        panic!("Unexpected end of tokens in print statement");
                    }
                    if let Token::Var(ref var_name) = tokens[*pos] {
                        *pos += 1;
                        Stmt::Print(var_name.clone())
                    } else {
                        panic!("Expected variable name in print statement");
                    }
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
                Some(Self::parse_block(tokens, pos))
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
        Add(Box<Expr>, Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        Mul(Box<Expr>, Box<Expr>),
        Div(Box<Expr>, Box<Expr>),
        Mod(Box<Expr>, Box<Expr>),
        Compare(Box<Expr>, CompareOp, Box<Expr>),
    }

    impl Expr {
        pub fn parse_expr(tokens: &[Token], pos: &mut usize) -> Expr {
            Self::parse_expr_bp(tokens, pos, 0)
        }

        fn parse_expr_bp(tokens: &[Token], pos: &mut usize, min_bp: u8) -> Expr {
            if *pos >= tokens.len() {
                panic!("Unexpected end of tokens");
            }

            let mut lhs = match &tokens[*pos] {
                Token::LParen => {
                    *pos += 1;
                    let expr = Self::parse_expr_bp(tokens, pos, 0);
                    if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                        panic!("Expected closing parenthesis");
                    }
                    *pos += 1; // Skip ')'
                    expr
                }
                Token::Number(n) => {
                    *pos += 1;
                    Expr::Number(*n)
                }
                Token::Var(name) => {
                    *pos += 1;
                    Expr::Var(name.clone())
                }
                _ => panic!("Unexpected token: {:?}", tokens[*pos]),
            };

            loop {
                if *pos >= tokens.len() {
                    break;
                }

                let op = match &tokens[*pos] {
                    Token::Add => "+",
                    Token::Sub => "-",
                    Token::Mul => "*",
                    Token::Div => "/",
                    Token::Mod => "%",
                    _ => break,
                };

                let (l_bp, r_bp) = match op {
                    "+" => (1, 2),
                    "-" => (1, 2),
                    "*" => (3, 4),
                    "/" => (3, 4),
                    "%" => (3, 4),
                    _ => break,
                };

                if l_bp < min_bp {
                    break;
                }

                *pos += 1;

                let rhs = Self::parse_expr_bp(tokens, pos, r_bp);

                lhs = match op {
                    "+" => Expr::Add(Box::new(lhs), Box::new(rhs)),
                    "-" => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                    "*" => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                    "/" => Expr::Div(Box::new(lhs), Box::new(rhs)),
                    "%" => Expr::Mod(Box::new(lhs), Box::new(rhs)),
                    _ => unreachable!(),
                };
            }

            lhs
        }

        pub fn parse_comparison_op(tokens: &[Token], pos: &mut usize) -> Expr {
            let lhs = Self::parse_expr_bp(tokens, pos, 0);

            let op = match &tokens[*pos] {
                Token::Eq => CompareOp::Eq,
                Token::Neq => CompareOp::Neq,
                Token::Lt => CompareOp::Lt,
                Token::Le => CompareOp::Le,
                Token::Gt => CompareOp::Gt,
                Token::Ge => CompareOp::Ge,
                _ => panic!("Unexpected token: {:?}", tokens[*pos]),
            };
            *pos += 1;

            let rhs = Self::parse_expr_bp(tokens, pos, 0);

            Expr::Compare(Box::new(lhs), op, Box::new(rhs))
        }
    }
}

mod vm {
    use crate::ast::{Expr, Stmt, CompareOp};
    use crate::lexer::tokenize;
    use crate::lexer::Token;
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::{self, prelude::*, BufReader, Error};
    use std::str::FromStr;

    pub struct Vm {
        instructions: Vec<Instruction>,
        context: HashMap<String, i32>,
    }

    impl Vm {
        pub fn new() -> Self {
            Vm {
                instructions: Vec::new(),
                context: HashMap::new(),
            }
        }

        pub fn parse_file(&mut self, filename: &str) -> Result<(), Error> {
            let file = File::open(filename)?;
            let mut reader = BufReader::new(file);
            let mut file_content = String::new();
            reader.read_to_string(&mut file_content)?;

            let tokens = tokenize(&file_content);
            let stmts = Stmt::parse_file(&tokens);
            for stmt in stmts {
                self.compile(&stmt);
            }
            Ok(())
        }

        pub fn compile(&mut self, stmt: &Stmt) {
            match stmt {
                Stmt::Let(var_name, expr) => {
                    self.compile_expr(expr);
                    self.instructions.push(Instruction::Store(var_name.clone()));
                }
                Stmt::Print(var_name) => {
                    self.instructions.push(Instruction::Load(var_name.clone()));
                    self.instructions.push(Instruction::Print);
                }
                Stmt::If(cond, true_branch, false_branch) => {
                    self.compile_expr(cond);
                    let jmp_if_false_index = self.instructions.len();
                    self.instructions.push(Instruction::JmpIfFalse(0)); // placeholder

                    for stmt in true_branch {
                        self.compile(stmt);
                    }

                    if let Some(false_branch) = false_branch {
                        let jmp_index = self.instructions.len();
                        self.instructions.push(Instruction::Jmp(0)); // placeholder

                        let false_branch_index = self.instructions.len();
                        self.instructions[jmp_if_false_index] = Instruction::JmpIfFalse(false_branch_index);

                        for stmt in false_branch {
                            self.compile(stmt);
                        }

                        let end_index = self.instructions.len();
                        self.instructions[jmp_index] = Instruction::Jmp(end_index);
                    } else {
                        let end_index = self.instructions.len();
                        self.instructions[jmp_if_false_index] = Instruction::JmpIfFalse(end_index);
                    }
                }
                _ => panic!("Invalid statement"),
            }
        }

        fn compile_expr(&mut self, expr: &Expr) {
            match expr {
                Expr::Number(n) => self.instructions.push(Instruction::Push(*n)),
                Expr::Var(name) => self.instructions.push(Instruction::Load(name.clone())),
                Expr::Add(left, right) => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.instructions.push(Instruction::Add);
                }
                Expr::Sub(left, right) => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.instructions.push(Instruction::Sub);
                }
                Expr::Mul(left, right) => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.instructions.push(Instruction::Mul);
                }
                Expr::Div(left, right) => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.instructions.push(Instruction::Div);
                }
                Expr::Mod(left, right) => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.instructions.push(Instruction::Mod);
                }
                Expr::Compare(left, op, right) => {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    match op {
                        CompareOp::Eq => self.instructions.push(Instruction::Eq),
                        CompareOp::Neq => self.instructions.push(Instruction::Neq),
                        CompareOp::Lt => self.instructions.push(Instruction::Lt),
                        CompareOp::Le => self.instructions.push(Instruction::Le),
                        CompareOp::Gt => self.instructions.push(Instruction::Gt),
                        CompareOp::Ge => self.instructions.push(Instruction::Ge),
                    }
                }
            }
        }

        pub fn execute(&mut self) -> i32 {
            let mut stack = Vec::new();
            let mut ip = 0; // instruction pointer

            while ip < self.instructions.len() {
                match &self.instructions[ip] {
                    Instruction::Push(n) => stack.push(*n),
                    Instruction::Load(var_name) => {
                        if let Some(value) = self.context.get(var_name) {
                            stack.push(*value);
                        } else {
                            panic!("Undefined variable: {}", var_name);
                        }
                    }
                    Instruction::Store(var_name) => {
                        let value = stack.pop().expect("no value to store");
                        self.context.insert(var_name.clone(), value);
                    }
                    Instruction::Add => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push(left + right);
                    }
                    Instruction::Sub => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push(left - right);
                    }
                    Instruction::Mul => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push(left * right);
                    }
                    Instruction::Div => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push(left / right);
                    }
                    Instruction::Mod => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push(left % right);
                    }
                    Instruction::Eq => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push((left == right) as i32);
                    }
                    Instruction::Neq => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push((left != right) as i32);
                    }
                    Instruction::Lt => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push((left < right) as i32);
                    }
                    Instruction::Le => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push((left <= right) as i32);
                    }
                    Instruction::Gt => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push((left > right) as i32);
                    }
                    Instruction::Ge => {
                        let right = stack.pop().expect("no right operand");
                        let left = stack.pop().expect("no left operand");
                        stack.push((left >= right) as i32);
                    }
                    Instruction::JmpIfFalse(target) => {
                        let value = stack.pop().expect("no value to test");
                        if value == 0 {
                            ip = *target;
                            continue;
                        }
                    }
                    Instruction::Jmp(target) => {
                        ip = *target;
                        continue;
                    }
                    Instruction::Print => {
                        let value = stack.pop().expect("no value to print");
                        println!("{}", value);
                    }
                }
                ip += 1;
            }
            stack.pop().unwrap_or(0)
        }
    }

    pub enum Instruction {
        Push(i32),         // 压栈
        Load(String),      // 加载变量
        Store(String),     // 存储变量
        Add,               // 加
        Sub,               // 减
        Mul,               // 乘
        Div,               // 除
        Mod,               // 取模
        Eq,                // 等于
        Neq,               // 不等于
        Lt,                // 小于
        Le,                // 小于等于
        Gt,                // 大于
        Ge,                // 大于等于
        JmpIfFalse(usize), // 条件跳转
        Jmp(usize),        // 无条件跳转
        Print,             // 打印
    }

    impl FromStr for Instruction {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let parts: Vec<&str> = s.split_whitespace().collect();
            match parts[0] {
                "push" => Ok(Instruction::Push(parts[1].parse().unwrap())),
                "Add" => Ok(Instruction::Add),
                _ => Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Invalid instruction",
                )),
            }
        }
    }
}

use crate::vm::Vm;

fn main() -> io::Result<()> {
    let path = "source.nl";

    let mut vm = Vm::new();
    vm.parse_file(&path)?;
    vm.execute();

    let fi_filepath = "source_if.nl";
    vm = Vm::new();
    vm.parse_file(&fi_filepath)?;
    vm.execute();

    Ok(())
}
