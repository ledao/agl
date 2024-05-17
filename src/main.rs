use std::fs::File;
use std::fmt;
use std::io::prelude::*;
use std::str::FromStr;
use std::io;

enum Expr {
    Number(i32),
    Add(Box<Expr>, Box<Expr>),
}

fn eval(e: Expr) -> i32 {
    match e {
        Expr::Number(n) => n,
        Expr::Add(left, right) => eval(*left) + eval(*right),
    }
}

enum Instruction {
    Push(i32), // 压栈
    Add,       // 加
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Push(n) => write!(f, "push {}", n),
            Instruction::Add => write!(f, "Add"),
        }
    }
}

impl FromStr for Instruction {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split_whitespace().collect();
        match parts[0] {
            "push" => Ok(Instruction::Push(parts[1].parse().unwrap())),
            "Add" => Ok(Instruction::Add),
            _ => Err(io::Error::new(io::ErrorKind::InvalidInput, "Invalid instruction")),
        }
    }
}

fn compile(expr: &Expr, instructions: &mut Vec<Instruction>) {
    match expr {
        Expr::Number(n) => instructions.push(Instruction::Push(*n)),
        Expr::Add(left, right) => {
            compile(left, instructions);
            compile(right, instructions);
            instructions.push(Instruction::Add);
        },
    }
}

fn execute(instructions: &mut Vec<Instruction>) -> i32 {
    let mut stack = Vec::new();
    for instruction in instructions {
        match instruction {
            Instruction::Push(n) => stack.push(*n),
            Instruction::Add => {
                let right = stack.pop().expect("no right operand");
                let left = stack.pop().expect("no left operand");
                stack.push(left + right);
            },
        }
    }
    stack.pop().expect("no result")
}

fn execute_file(filename: &str) -> Result<i32, std::io::Error> {
    let file  = File::open(filename).unwrap();
    let reader = std::io::BufReader::new(file);
    let mut instructions = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let instruction = line.parse()?;
        instructions.push(instruction);
    }
    Ok(execute(&mut instructions))
}

fn main() {
    let expr = Expr::Add(Box::new(Expr::Number(1)), Box::new(Expr::Number(2)));

    let mut instructions = Vec::new();
    compile(&expr, &mut instructions);

    let mut file = File::create("instructions.txt").unwrap();
    for instruction in &instructions {
        writeln!(file, "{}", instruction).unwrap();
    }

    // let result = execute(&mut instructions);
    let result = execute_file("instructions.txt").unwrap();

    println!("1 + 2 = {}", result);


}
