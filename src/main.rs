use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

enum Expr {
    Number(i32),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
}

enum Stmt {
    Let(String, Expr),
    Print(String),
}

enum Instruction {
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
    type Err = io::Error;

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

fn parse_line(line: &str) -> Stmt {
    let tokens: Vec<&str> = line.split_whitespace().collect();
    match tokens[0] {
        "let" => {
            let var_name = tokens[1].to_string();
            let expr = parse_expr(&tokens[3..]);
            Stmt::Let(var_name, expr)
        }
        "print" => {
            let var_name = tokens[1].to_string();
            Stmt::Print(var_name)
        }
        _ => panic!("Invalid statement"),
    }
}

fn parse_expr(tokens: &[&str]) -> Expr {
    if tokens.len() == 1 {
        if let Ok(n) = tokens[0].parse::<i32>() {
            return Expr::Number(n);
        } else {
            return Expr::Var(tokens[0].to_string());
        }
    }
    let left = Box::new(parse_expr(&tokens[..1]));
    let right = Box::new(parse_expr(&tokens[2..3]));
    match tokens[1] {
        "+" => Expr::Add(left, right),
        "-" => Expr::Sub(left, right),
        "*" => Expr::Mul(left, right),
        "/" => Expr::Div(left, right),
        "%" => Expr::Mod(left, right),
        _ => panic!("Invalid operator"),
    }
}

fn compile(stmt: &Stmt, instructions: &mut Vec<Instruction>) {
    match stmt {
        Stmt::Let(var_name, expr) => {
            compile_expr(expr, instructions);
            instructions.push(Instruction::Store(var_name.clone()));
        }
        Stmt::Print(var_name) => {
            instructions.push(Instruction::Load(var_name.clone()));
            instructions.push(Instruction::Print);
        }
    }
}

fn compile_expr(expr: &Expr, instructions: &mut Vec<Instruction>) {
    match expr {
        Expr::Number(n) => instructions.push(Instruction::Push(*n)),
        Expr::Var(name) => instructions.push(Instruction::Load(name.clone())),
        Expr::Add(left, right) => {
            compile_expr(left, instructions);
            compile_expr(right, instructions);
            instructions.push(Instruction::Add);
        }
        Expr::Sub(left, right) => {
            compile_expr(left, instructions);
            compile_expr(right, instructions);
            instructions.push(Instruction::Sub);
        }
        Expr::Mul(left, right) => {
            compile_expr(left, instructions);
            compile_expr(right, instructions);
            instructions.push(Instruction::Mul);
        }
        Expr::Div(left, right) => {
            compile_expr(left, instructions);
            compile_expr(right, instructions);
            instructions.push(Instruction::Div);
        }
        Expr::Mod(left, right) => {
            compile_expr(left, instructions);
            compile_expr(right, instructions);
            instructions.push(Instruction::Mod);
        }
    }
}

fn execute(instructions: &mut Vec<Instruction>, context: &mut HashMap<String, i32>) -> i32 {
    let mut stack = Vec::new();
    for instruction in instructions {
        match instruction {
            Instruction::Push(n) => stack.push(*n),
            Instruction::Load(var_name) => {
                if let Some(value) = context.get(var_name) {
                    stack.push(*value);
                } else {
                    panic!("Undefined variable: {}", var_name);
                }
            }
            Instruction::Store(var_name) => {
                let value = stack.pop().expect("no value to store");
                context.insert(var_name.clone(), value);
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

fn main() -> io::Result<()> {
    let path = "source.nl";
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut instructions = Vec::new();
    let mut context = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let stmt = parse_line(&line);
        compile(&stmt, &mut instructions);
    }

    execute(&mut instructions, &mut context);

    Ok(())
}
