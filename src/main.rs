use std::fs::File;
use std::io::{self, prelude::*, BufReader};

mod ast {
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
        pub fn parse_expr(tokens: &[&str]) -> Expr {
            let mut pos = 0;
            Self::parse_expr_bp(tokens, &mut pos, 0)
        }

        fn parse_expr_bp(tokens: &[&str], pos: &mut usize, min_bp: u8) -> Expr {
            if *pos >= tokens.len() {
                panic!("Unexpected end of tokens");
            }

            let mut lhs = match tokens[*pos] {
                "(" => {
                    *pos += 1;
                    let expr = Self::parse_expr_bp(tokens, pos, 0);
                    if *pos >= tokens.len() || tokens[*pos] != ")" {
                        panic!("Expected closing parenthesis");
                    }
                    *pos += 1; // Skip ')'
                    expr
                }
                token => {
                    *pos += 1;
                    if let Ok(n) = token.parse::<i32>() {
                        Expr::Number(n)
                    } else {
                        Expr::Var(token.to_string())
                    }
                }
            };

            loop {
                if *pos >= tokens.len() {
                    break;
                }

                let op = match tokens.get(*pos) {
                    Some(&op) => op,
                    None => break,
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

        pub fn parse_line(line: &str) -> Stmt {
            let tokens: Vec<&str> = line.split_whitespace().collect();
            if tokens.is_empty() {
                panic!("Empty line");
            }
            match tokens[0] {
                "let" => {
                    if tokens.len() < 4 {
                        panic!("Invalid let statement");
                    }
                    let var_name = tokens[1].to_string();
                    let expr = Self::parse_expr(&tokens[3..]);
                    Stmt::Let(var_name, expr)
                }
                "print" => {
                    if tokens.len() != 2 {
                        panic!("Invalid print statement");
                    }
                    let var_name = tokens[1].to_string();
                    Stmt::Print(var_name)
                }
                _ => panic!("Invalid statement"),
            }
        }
    }
}

mod vm {
    use crate::ast::{Expr, Stmt};
    use std::collections::HashMap;
    use std::io::{self, Error};
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

use crate::ast::Expr;
use crate::vm::Vm;

fn main() -> io::Result<()> {
    let path = "source.nl";
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut vm = Vm::new();

    for line in reader.lines() {
        let line = line?;
        let stmt = Expr::parse_line(&line);
        vm.compile(&stmt);
    }

    vm.execute();

    Ok(())
}
