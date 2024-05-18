use std::fs::File;
use std::io::{self, prelude::*, BufReader};

mod lexer {
    #[derive(Debug, PartialEq)]
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
        Let,
        Print,
        Equal,
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
                '=' => {
                    chars.next();
                    tokens.push(Token::Equal);
                }
                ' ' | '\t' | '\n' | '\r' => {
                    chars.next();
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
    pub enum Expr {
        Number(i32),
        Var(String),
        Add(Box<Expr>, Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        Mul(Box<Expr>, Box<Expr>),
        Div(Box<Expr>, Box<Expr>),
        Mod(Box<Expr>, Box<Expr>),
    }

    #[derive(Debug)]
    pub enum Stmt {
        Let(String, Expr),
        Print(String),
    }

    impl Expr {
        pub fn parse_expr(tokens: &[Token]) -> Expr {
            let mut pos = 0;
            Self::parse_expr_bp(tokens, &mut pos, 0)
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

        pub fn parse_line(tokens: &[Token]) -> Stmt {
            if tokens.is_empty() {
                panic!("Empty line");
            }

            match tokens[0] {
                Token::Let => {
                    if tokens.len() < 4 {
                        panic!("Invalid let statement");
                    }
                    if let Token::Var(ref var_name) = tokens[1] {
                        if tokens[2] != Token::Equal {
                            panic!("Expected '=' in let statement");
                        }
                        let expr = Self::parse_expr(&tokens[3..]);
                        Stmt::Let(var_name.clone(), expr)
                    } else {
                        panic!("Expected variable name in let statement");
                    }
                }
                Token::Print => {
                    if tokens.len() != 2 {
                        panic!("Invalid print statement");
                    }
                    if let Token::Var(ref var_name) = tokens[1] {
                        Stmt::Print(var_name.clone())
                    } else {
                        panic!("Expected variable name in print statement");
                    }
                }
                _ => panic!("Invalid statement"),
            }
        }
    }
}

mod vm {
    use crate::ast::{Expr, Stmt};
    use crate::lexer::tokenize;
    use std::collections::HashMap;
    use std::fs::File;
    use std::str::FromStr;
    use std::io::{self, Error, prelude::*, BufReader};


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
            let reader = BufReader::new(file);

            for line in reader.lines() {
                let line = line?;
                let tokens = tokenize(&line);
                let stmt = Expr::parse_line(&tokens);
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
            }
        }

        pub fn execute(&mut self) -> i32 {
            let mut stack = Vec::new();
            for instruction in &self.instructions {
                match instruction {
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
                    Instruction::Print => {
                        let value = stack.pop().expect("no value to print");
                        println!("{}", value);
                    }
                }
            }
            stack.pop().unwrap_or(0)
        }
    }

    pub enum Instruction {
        Push(i32),     // 压栈
        Load(String),  // 加载变量
        Store(String), // 存储变量
        Add,           // 加
        Sub,           // 减
        Mul,           // 乘
        Div,           // 除
        Mod,           // 取模
        Print,         // 打印
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

    Ok(())
}

