

enum Expr {
    Number(i32),
    Add(Box<Expr>, Box<Expr>),
}

fn eval(e: Expr) -> i32 {
    match  e {
        Expr::Number(n) => n,
        Expr::Add(left, right) => eval(*left) + eval(*right),
    }
}


fn main() {
    let expr = Expr::Add(Box::new(Expr::Number(1)), Box::new(Expr::Number(2)));
    let result = eval(expr);

    println!("1 + 2 = {}", result);
}
