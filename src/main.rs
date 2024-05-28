mod lexer {
    use core::panic;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Int64(i64),
        Bool(bool),
        Float(f64),
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
        Colon,
        Comma,
        Dot,
        Let,
        Mut,
        Print,
        Println,
        Assign,
        If,
        Else,
        Struct,
        Eq,
        Neq,
        Lt,
        Gt,
        Le,
        Ge,
        NewLine,
        Fn,
        Return,
        Arrow,
        Type(String),
        Assert,
    }

    pub fn tokenize(input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                '0'..='9' => {
                    let mut number = String::new();
                    let mut has_dot = false;
                    while let Some(&ch) = chars.peek() {
                        if ch.is_numeric() {
                            number.push(ch);
                            chars.next();
                        } else if ch == '.' {
                            number.push(ch);
                            if has_dot {
                                panic!("Invalid number, 2 dots");
                            }
                            has_dot = true;
                        } else {
                            break;
                        }
                    }
                    if has_dot {
                        tokens.push(Token::Float(number.parse().unwrap()));
                    } else {
                        tokens.push(Token::Int64(number.parse().unwrap()));
                    }
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut ident = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            ident.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(match ident.as_str() {
                        "let" => Token::Let,
                        "mut" => Token::Mut,
                        "print" => Token::Print,
                        "println" => Token::Println,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "struct" => Token::Struct,
                        "fn" => Token::Fn,
                        "return" => Token::Return,
                        "int" => Token::Type("int".to_string()),
                        "float" => Token::Type("float".to_string()),
                        "bool" => Token::Type("bool".to_string()),
                        "string" => Token::Type("string".to_string()),
                        "true" => Token::Bool(true),
                        "false" => Token::Bool(false),
                        "assert" => Token::Assert,
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
                    if let Some(&'>') = chars.peek() {
                        chars.next();
                        tokens.push(Token::Arrow);
                    } else {
                        tokens.push(Token::Sub);
                    }
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
                '#' => {
                    chars.next();
                    while let Some(&ch) = chars.peek() {
                        if ch == '\n' {
                            chars.next();
                            break;
                        } else if ch == '\r' {
                            chars.next();
                            chars.next();
                            break;
                        } else {
                            chars.next();
                        }
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

    #[derive(Debug, Clone)]
    pub enum ValueType {
        Void,
        Int,
        Float,
        String,
        Bool,
    }

    #[derive(Debug, Clone)]
    pub enum Stmt {
        Let(String, ValueType, Expr),
        Mut(String, ValueType, Expr),
        Print(Expr),
        Println(Expr),
        Assert(Expr, String),
        If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
        StructDef(String, Vec<(String, ValueType)>),
        StructInstance(String, String, Vec<(String, Expr)>),
        FnDef(String, Vec<(String, ValueType)>, Vec<Stmt>, ValueType),
        Return(Expr),
        Assign(String, Expr),
    }

    #[derive(Debug, Clone)]
    pub enum Expr {
        Int64(i64),
        Float(f64),
        Bool(bool),
        Var(String),
        Str(String),
        BinOp(Box<Expr>, BinOp, Box<Expr>),
        StructFieldAccess(Box<Expr>, String, String), // struct instalce name, field_name, struct type name
        FnCall(String, Vec<Expr>),
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
                    let mut var_type = ValueType::Void;
                    if let Token::Var(ref var_name) = tokens[*pos] {
                        *pos += 1;
                        if Token::Colon == tokens[*pos] {
                            *pos += 1;
                            if *pos >= tokens.len() {
                                panic!("Expected type name in let statement");
                            }
                            if let Token::Type(ref type_name) = tokens[*pos] {
                                var_type = match type_name.as_str() {
                                    "int" => ValueType::Int,
                                    "float" => ValueType::Float,
                                    "string" => ValueType::String,
                                    "bool" => ValueType::Bool,
                                    _ => panic!("Unknown type name: {}", type_name),
                                };
                            } else {
                                panic!("Expected type name in let statement")
                            }
                            *pos += 1;
                        }
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
                                    match var_type {
                                        ValueType::Void => {
                                            return Stmt::Let(var_name.clone(), var_type, expr);
                                        }
                                        ValueType::Bool => {
                                            if let Expr::Bool(_) = expr {
                                                return Stmt::Let(var_name.clone(), var_type, expr);
                                            } else {
                                                panic!("Expected bool value in let statement")
                                            }
                                        }
                                        ValueType::Int => {
                                            if let Expr::Int64(_) = expr {
                                                return Stmt::Let(var_name.clone(), var_type, expr);
                                            } else {
                                                panic!("Expected int value in let statement")
                                            }
                                        }
                                        ValueType::Float => {
                                            if let Expr::Float(_) = expr {
                                                return Stmt::Let(var_name.clone(), var_type, expr);
                                            } else {
                                                panic!("Expected float value in let statement")
                                            }
                                        }
                                        ValueType::String => {
                                            if let Expr::Str(_) = expr {
                                                return Stmt::Let(var_name.clone(), var_type, expr);
                                            } else {
                                                panic!("Expected string value in let statement")
                                            }
                                        }
                                    };
                                }
                            }
                        }
                        panic!("Expected expression in let statement")
                    } else {
                        panic!("Expected variable name in let statement");
                    }
                }
                Token::Mut => {
                    *pos += 1;
                    if *pos >= tokens.len() {
                        panic!("Unexpected end of tokens in mut statement");
                    }
                    let mut var_type = ValueType::Void;
                    if let Token::Var(ref var_name) = tokens[*pos] {
                        *pos += 1;
                        if Token::Colon == tokens[*pos] {
                            *pos += 1;
                            if *pos >= tokens.len() {
                                panic!("Expected type name in mut statement");
                            }
                            if let Token::Type(ref type_name) = tokens[*pos] {
                                var_type = match type_name.as_str() {
                                    "int" => ValueType::Int,
                                    "float" => ValueType::Float,
                                    "string" => ValueType::String,
                                    "bool" => ValueType::Bool,
                                    _ => panic!("Unknown type name: {}", type_name),
                                };
                            } else {
                                panic!("Expected type name in mut statement")
                            }
                            *pos += 1;
                        }
                        if *pos >= tokens.len() || tokens[*pos] != Token::Assign {
                            panic!("Expected '=' in mut statement");
                        }
                        *pos += 1;
                        let expr = Expr::parse_expr(tokens, pos);
                        return Stmt::Mut(var_name.clone(), var_type, expr);
                    } else {
                        panic!("Expected variable name in mut statement");
                    }
                }
                Token::Var(ref var_name) => {
                    if *pos + 1 < tokens.len() && tokens[*pos + 1] == Token::Assign {
                        *pos += 2;
                        let expr = Expr::parse_expr(tokens, pos);
                        return Stmt::Assign(var_name.clone(), expr);
                    } else {
                        unimplemented!()
                    }
                }
                Token::Print => {
                    *pos += 1;
                    if *pos >= tokens.len() || tokens[*pos] != Token::LParen {
                        panic!("Expected '(' in print statement");
                    }
                    *pos += 1;
                    let expr = Expr::parse_expr(tokens, pos);
                    if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                        panic!("Expected ')' in print statement");
                    }
                    *pos += 1;
                    Stmt::Print(expr)
                }
                Token::Println => {
                    *pos += 1;
                    if *pos >= tokens.len() || tokens[*pos] != Token::LParen {
                        panic!("Expected '(' in print statement");
                    }
                    *pos += 1;
                    let expr = Expr::parse_expr(tokens, pos);
                    if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                        panic!("Expected ')' in print statement");
                    }
                    *pos += 1;
                    Stmt::Println(expr)
                }
                Token::Assert => {
                    *pos += 1;
                    if *pos >= tokens.len() || tokens[*pos] != Token::LParen {
                        panic!("Expected '(' in print statement");
                    }
                    *pos += 1;
                    let expr = Expr::parse_comparison_op(tokens, pos);
                    if *pos >= tokens.len() {
                        panic!(
                            "Position out of range, current pos {}, total length {}",
                            *pos,
                            tokens.len()
                        );
                    }
                    if tokens[*pos] == Token::LParen {
                        *pos += 1;
                        Stmt::Assert(expr, "".to_string())
                    } else if tokens[*pos] == Token::Comma {
                        *pos += 1;
                        if let Expr::Str(s) = Expr::parse_primary(tokens, pos) {
                            *pos += 1;
                            Stmt::Assert(expr, s.clone())
                        } else {
                            panic!("Expected string in assert function last parameter")
                        }
                    } else {
                        panic!("Expected ',' in function definition")
                    }
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
                                if let Token::Type(ref field_type) = tokens[*pos] {
                                    *pos += 1;
                                    let final_type = match field_type.as_str() {
                                        "int" => ValueType::Int,
                                        "float" => ValueType::Float,
                                        "bool" => ValueType::Bool,
                                        "string" => ValueType::String,
                                        _ => panic!(
                                            "Unknown type in struct definition: {}",
                                            field_type
                                        ),
                                    };
                                    fields.push((field_name.clone(), final_type));
                                } else {
                                    panic!(
                                        "Expected type name in struct definition, {:?}",
                                        tokens[*pos]
                                    );
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
                Token::Fn => {
                    *pos += 1;
                    if let Token::Var(ref func_name) = tokens[*pos] {
                        *pos += 1;
                        if *pos >= tokens.len() || tokens[*pos] != Token::LParen {
                            panic!("Expected '(' in function definition");
                        }
                        *pos += 1;
                        let mut params = Vec::new();
                        while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                            if Token::NewLine == tokens[*pos] {
                                *pos += 1;
                                continue;
                            }
                            let param_name = if let Token::Var(ref param_name) = tokens[*pos] {
                                param_name
                            } else {
                                panic!(
                                    "Expected parameter name in function definition, {:?}",
                                    tokens[*pos]
                                );
                            };
                            *pos += 1;

                            let mut value_type = ValueType::Void;
                            if tokens[*pos] == Token::Colon {
                                *pos += 1;
                                value_type = if let Token::Type(ref param_type) = tokens[*pos] {
                                    *pos += 1;
                                    match param_type.as_str() {
                                        "int" => ValueType::Int,
                                        "float" => ValueType::Float,
                                        "bool" => ValueType::Bool,
                                        "string" => ValueType::String,
                                        _ => panic!(
                                            "Unknown type in function definition: {}",
                                            param_type
                                        ),
                                    }
                                } else {
                                    panic!("Expected type name in function definition")
                                }
                            };

                            if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                                *pos += 1;
                            }
                            params.push((param_name.clone(), value_type));
                        }
                        if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                            panic!("Expected ')' in function definition");
                        }
                        *pos += 1;
                        let mut return_type = ValueType::Void;
                        if tokens[*pos] == Token::Colon {
                            *pos += 1;
                            if let Token::Type(ref rtype) = tokens[*pos] {
                                return_type = match rtype.as_str() {
                                    "int" => ValueType::Int,
                                    "float" => ValueType::Float,
                                    "bool" => ValueType::Bool,
                                    "string" => ValueType::String,
                                    _ => panic!("Unknown type in function definition: {}", rtype),
                                };
                            } else {
                                panic!("Expected type name in function definition")
                            }
                        }

                        *pos += 1;
                        if *pos >= tokens.len() || tokens[*pos] != Token::LBrace {
                            panic!("Expected '{{' in function definition");
                        }
                        *pos += 1;
                        let mut body = Vec::new();
                        while *pos < tokens.len() && tokens[*pos] != Token::RBrace {
                            if Token::NewLine == tokens[*pos] {
                                *pos += 1;
                                continue;
                            }
                            body.push(Self::parse_stmt(tokens, pos));
                        }
                        if *pos >= tokens.len() || tokens[*pos] != Token::RBrace {
                            panic!("Expected '}}' in function definition");
                        }
                        *pos += 1;
                        return Stmt::FnDef(func_name.clone(), params, body, return_type);
                    } else {
                        panic!("Expected function name in function definition");
                    }
                }
                Token::Return => {
                    *pos += 1;
                    let expr = Expr::parse_expr(tokens, pos);
                    Stmt::Return(expr)
                }
                _ => panic!("Unexpected token: {:?}", tokens[*pos]),
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
            match tokens[*pos] {
                Token::Int64(n) => {
                    *pos += 1;
                    Expr::Int64(n)
                }
                Token::Bool(b) => {
                    *pos += 1;
                    Expr::Bool(b)
                }
                Token::Float(f) => {
                    *pos += 1;
                    Expr::Float(f)
                }
                Token::Var(ref var_name) => {
                    *pos += 1;
                    if *pos < tokens.len() && tokens[*pos] == Token::LParen {
                        *pos += 1;
                        let mut args = Vec::new();
                        while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                            args.push(Self::parse_expr(tokens, pos));
                            if *pos < tokens.len() && tokens[*pos] == Token::Comma {
                                *pos += 1;
                            }
                        }
                        if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                            panic!("Expected ')' after function arguments");
                        }
                        *pos += 1;
                        return Expr::FnCall(var_name.clone(), args);
                    } else if *pos < tokens.len() && tokens[*pos] == Token::Dot {
                        *pos += 1;
                        if let Token::Var(field_name) = &tokens[*pos] {
                            *pos += 1;
                            return Expr::StructFieldAccess(
                                Box::new(Expr::Var(var_name.clone())),
                                field_name.clone(),
                                "".to_string(),
                            );
                        } else {
                            panic!("Expected field name after '.' in struct field access");
                        }
                    }
                    Expr::Var(var_name.clone())
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

    #[derive(Debug, Clone)]
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
    use crate::ast::{BinOp, Expr, Stmt, ValueType};
    use std::{
        collections::HashMap,
        fmt::{Display, Formatter},
    };

    pub struct VM {
        global_variables: HashMap<String, (Value, Mutabble)>,
        structs: HashMap<String, StructDef>,
        local_variables: Vec<HashMap<String, (Value, Mutabble)>>,
    }

    #[derive(Debug, Clone)]
    pub enum Value {
        Int64(i64),
        Float(f64),
        Bool(bool),
        Str(String),
        StructInstance(HashMap<String, Value>, String, String), // fields, var_name, struct_name
        Fn(Vec<(String, ValueType)>, Vec<Stmt>),
    }

    impl Display for Value {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Int64(n) => write!(f, "{}", n),
                Self::Float(n) => write!(f, "{}", n),
                Self::Bool(b) => write!(f, "{}", b),
                Self::Str(s) => write!(f, "\"{}\"", s),
                Self::StructInstance(fields, var_name, struct_name) => {
                    write!(f, "{} {} {{", struct_name, var_name)?;
                    for (field_name, field_value) in fields {
                        write!(f, " {}:{} ", field_name, field_value)?;
                    }
                    write!(f, "}}")?;
                    Ok(())
                }
                Self::Fn(_params, _stmts) => write!(f, "<fn>"),
            }
        }
    }

    #[derive(Clone)]
    enum Mutabble {
        Let,
        Mut,
    }

    #[derive(Debug, Clone)]
    pub struct StructDef {
        fields: HashMap<String, ValueType>,
    }

    impl VM {
        pub fn new() -> Self {
            VM {
                global_variables: HashMap::new(),
                structs: HashMap::new(),
                local_variables: vec![HashMap::new()],
            }
        }

        pub fn run(&mut self, stmts: &[Stmt]) {
            for stmt in stmts {
                self.execute_stmt(stmt);
            }
        }

        fn execute_stmt(&mut self, stmt: &Stmt) {
            match stmt {
                Stmt::Let(var_name, _var_type, expr) => {
                    let value = self.evaluate_expr(expr);
                    self.current_env().insert(var_name.clone(), (value, Mutabble::Let));
                }
                Stmt::Mut(var_name, _var_type, expr) => {
                    let value = self.evaluate_expr(expr);
                    self.current_env().insert(var_name.clone(), (value, Mutabble::Mut));
                }
                Stmt::Print(expr) => {
                    let value = self.evaluate_expr(expr);
                    print!("{}", value);
                }
                Stmt::Println(expr) => {
                    let value = self.evaluate_expr(expr);
                    println!("{}", value);
                }
                Stmt::Assert(expr, s) => {
                    if !self.evaluate_cond(expr) {
                        println!("assert fails, {:?}, {}", expr, s);
                    }
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
                    if let Some(struct_def) = self.structs.get(struct_name).cloned() {
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
                        self.current_env().insert(
                            var_name.clone(),
                            (Value::StructInstance(instance, var_name.clone(), struct_name.clone()), Mutabble::Let),
                        );
                    } else {
                        panic!("Struct '{}' not defined", struct_name);
                    }
                }
                Stmt::FnDef(name, params, body, _return_type) => {
                    self.global_variables
                        .insert(name.clone(), (Value::Fn(params.clone(), body.clone()), Mutabble::Let));
                }
                Stmt::Return(expr) => {
                    let value = self.evaluate_expr(expr);
                    self.current_env().insert("return".to_string(), (value, Mutabble::Let));
                }
                Stmt::Assign(var_name, expr) => {
                    let value = self.evaluate_expr(expr);
                    if let Some((_value, mutable)) = self.current_env().get(var_name) {
                        match mutable {
                            Mutabble::Let => panic!("Variable {} is not mutable", var_name),
                            _ => {},
                        }
                    }
                    self.current_env().insert(var_name.clone(), (value, Mutabble::Mut));
                }
            }
        }

        fn evaluate_expr(&mut self, expr: &Expr) -> Value {
            match expr {
                Expr::Int64(n) => Value::Int64(*n),
                Expr::Float(f) => Value::Float(*f),
                Expr::Bool(b) => Value::Bool(*b),
                Expr::Var(var_name) => self.lookup_var(var_name),
                Expr::Str(s) => Value::Str(s.clone()),
                Expr::BinOp(lhs, op, rhs) => {
                    let lhs_val = self.evaluate_expr(lhs);
                    let rhs_val = self.evaluate_expr(rhs);
                    match (lhs_val, rhs_val, op) {
                        (Value::Int64(l), Value::Int64(r), BinOp::Add) => Value::Int64(l + r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Sub) => Value::Int64(l - r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Mul) => Value::Int64(l * r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Div) => Value::Int64(l / r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Mod) => Value::Int64(l % r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Eq) => Value::Bool(l == r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Neq) => Value::Bool(l != r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Lt) => Value::Bool(l < r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Gt) => Value::Bool(l > r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Le) => Value::Bool(l <= r),
                        (Value::Int64(l), Value::Int64(r), BinOp::Ge) => Value::Bool(l >= r),
                        _ => panic!("Invalid binary operation"),
                    }
                }
                Expr::StructFieldAccess(expr, field_name, _struct_name) => {

                    if let Value::StructInstance(instance, _var_name, _struct_name) =
                        self.evaluate_expr(expr)
                    {
                        if let Some(value) = instance.get(field_name) {
                            value.clone()
                        } else {
                            panic!("Field '{}' not found in struct instance", field_name);
                        }
                    } else {
                        panic!("Expected struct instance in field access");
                    }
                }
                Expr::FnCall(func_name, args) => {
                    if let Value::Fn(params, body) = self.lookup_var(func_name) {
                        if params.len() != args.len() {
                            panic!("Argument count mismatch in function call to {}", func_name);
                        }
                        let mut new_env = HashMap::new();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            new_env.insert(param.0.clone(), (self.evaluate_expr(arg), Mutabble::Let));
                        }
                        self.local_variables.push(new_env);
                        self.run(&body);
                        let ret_val = self
                            .local_variables
                            .pop()
                            .unwrap()
                            .remove("return")
                            .unwrap_or((Value::Int64(0), Mutabble::Let));
                        ret_val.0
                    } else {
                        panic!("Expected function in function call to {}", func_name)
                    }
                }
            }
        }

        fn lookup_var(&self, var_name: &str) -> Value {
            for env in self.local_variables.iter().rev() {
                if let Some(value) = env.get(var_name) {
                    return value.0.clone();
                }
            }
            let a = self.global_variables
                .get(var_name)
                .cloned()
                .unwrap_or_else(|| panic!("Undefined variable '{}'", var_name));
            a.0
        }

        fn evaluate_cond(&mut self, expr: &Expr) -> bool {
            match self.evaluate_expr(expr) {
                Value::Bool(b) => b,
                _ => panic!("Expected numeric value in condition, {:?}", expr),
            }
        }
        fn current_env(&mut self) -> &mut HashMap<String, (Value, Mutabble)> {
            self.local_variables.last_mut().unwrap()
        }
    }
}

use std::fs::File;
use std::io::Read;

fn main() {
    let mut source = String::new();
    let mut source_file = File::open("source.agl").unwrap();
    _ = source_file.read_to_string(&mut source);

    let tokens = lexer::tokenize(&mut source);
    // println!("{:?}", tokens);

    let ast = ast::Stmt::parse_tokens(&tokens);
    // println!("{:?}", ast);

    let mut vm = vm::VM::new();
    vm.run(&ast);
}
