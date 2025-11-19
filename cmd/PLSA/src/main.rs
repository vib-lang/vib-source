// src/main.rs
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

#[derive(Parser)]
#[grammar = "vib.pest"]
struct VibParser;

#[derive(Debug, Clone)]
enum Type {
    Number,
    String,
    Function(Vec<Type>, Box<Type>),
    Any, // For dynamic or unknown
}

#[derive(Debug)]
enum Expr {
    Literal(Literal),
    Variable(String),
    Binary(Box<Expr>, Op, Box<Expr>),
    Assign(String, Box<Expr>), // Changed to String for name
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug)]
enum Literal {
    Number(f64),
    String(String),
}

#[derive(Debug)]
enum Op {
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug)]
enum Stmt {
    VarDecl(String, Expr),
    ExprStmt(Expr),
    Function(String, Vec<String>, Vec<Stmt>, Type), // Added return type, but for now Any
    Return(Expr),
    Require(String),
    Embedded(String, String),
}

struct Scope {
    types: HashMap<String, Type>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: plsa <input.vib>");
        process::exit(1);
    }

    let filename = &args[1];
    let source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            process::exit(1);
        }
    };

    let pairs = match VibParser::parse(Rule::program, &source) {
        Ok(mut p) => p.next().unwrap().into_inner(),
        Err(e) => {
            eprintln!("Parse error:\n{}", e);
            process::exit(1);
        }
    };

    let ast = match build_ast(pairs) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("AST build error: {}", e);
            process::exit(1);
        }
    };

    let mut global_scope = Scope {
        types: HashMap::new(),
    };
    // Predefine built-ins like write
    global_scope.types.insert("write".to_string(), Type::Function(vec![Type::Any], Box::new(Type::Any)));

    match analyze(&ast, &mut global_scope, &mut Vec::new()) {
        Ok(_) => println!("Semantic analysis passed."),
        Err(e) => {
            eprintln!("Semantic error: {}", e);
            process::exit(1);
        }
    }

    // Output AST for now
    println!("{:#?}", ast);

    // In future, serialize AST to .object or something
}

fn build_ast(pairs: Pairs<Rule>) -> Result<Vec<Stmt>, String> {
    let mut ast = Vec::new();
    for pair in pairs {
        if pair.as_rule() == Rule::COMMENT || pair.as_rule() == Rule::WHITESPACE {
            continue;
        }
        ast.push(build_stmt(pair)?);
    }
    Ok(ast)
}

fn build_stmt(pair: Pair<Rule>) -> Result<Stmt, String> {
    match pair.as_rule() {
        Rule::var_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let expr = build_expr(inner.next().unwrap())?;
            Ok(Stmt::VarDecl(name, expr))
        }
        Rule::function_decl => {
            let mut inner = pair.into_inner();
            inner.next(); // skip 'function'
            let name = inner.next().unwrap().as_str().to_string();
            let params_pair = inner.next().unwrap().into_inner();
            let mut params = Vec::new();
            for p in params_pair {
                params.push(p.as_str().to_string());
            }
            let body_pair = inner.next().unwrap();
            let body = build_ast(body_pair.into_inner())?;
            Ok(Stmt::Function(name, params, body, Type::Any)) // Placeholder type
        }
        Rule::return_stmt => {
            let mut inner = pair.into_inner();
            let expr = build_expr(inner.next().unwrap())?;
            Ok(Stmt::Return(expr))
        }
        Rule::require_stmt => {
            let mut inner = pair.into_inner();
            let mod_name = inner.next().unwrap().as_str().to_string();
            Ok(Stmt::Require(mod_name))
        }
        Rule::embedded => {
            let mut inner = pair.into_inner();
            let lang = inner.next().unwrap().as_str().to_string();
            let code = inner.next().unwrap().as_str().to_string();
            Ok(Stmt::Embedded(lang, code))
        }
        Rule::expr_stmt => {
            let mut inner = pair.into_inner();
            let expr = build_expr(inner.next().unwrap())?;
            Ok(Stmt::ExprStmt(expr))
        }
        _ => Err(format!("Unexpected rule in stmt: {:?}", pair.as_rule())),
    }
}

fn build_expr(pair: Pair<Rule>) -> Result<Expr, String> {
    match pair.as_rule() {
        Rule::assign => {
            let mut inner = pair.into_inner();
            let left = build_expr(inner.next().unwrap())?;
            if let Some(_) = inner.next() { // equals
                let right = build_expr(inner.next().unwrap())?;
                if let Expr::Variable(name) = left {
                    Ok(Expr::Assign(name, Box::new(right)))
                } else {
                    Err("Assignment left side must be variable".to_string())
                }
            } else {
                Ok(left)
            }
        }
        Rule::additive => {
            let mut inner = pair.into_inner();
            let mut expr = build_expr(inner.next().unwrap())?;
            while let Some(op_pair) = inner.next() {
                let op = match op_pair.as_rule() {
                    Rule::plus => Op::Plus,
                    Rule::minus => Op::Minus,
                    _ => return Err(format!("Unexpected op: {:?}", op_pair)),
                };
                let right = build_expr(inner.next().unwrap())?;
                expr = Expr::Binary(Box::new(expr), op, Box::new(right));
            }
            Ok(expr)
        }
        Rule::multiplicative => {
            let mut inner = pair.into_inner();
            let mut expr = build_expr(inner.next().unwrap())?;
            while let Some(op_pair) = inner.next() {
                let op = match op_pair.as_rule() {
                    Rule::star => Op::Star,
                    Rule::slash => Op::Slash,
                    _ => return Err(format!("Unexpected op: {:?}", op_pair)),
                };
                let right = build_expr(inner.next().unwrap())?;
                expr = Expr::Binary(Box::new(expr), op, Box::new(right));
            }
            Ok(expr)
        }
        Rule::primary => {
            let mut inner = pair.into_inner();
            let primary_pair = inner.next().unwrap();
            match primary_pair.as_rule() {
                Rule::number => Ok(Expr::Literal(Literal::Number(primary_pair.as_str().parse::<f64>().map_err(|_| "Invalid number".to_string())?))),
                Rule::string => Ok(Expr::Literal(Literal::String(primary_pair.as_str()[1..primary_pair.as_str().len()-1].to_string()))), // Strip quotes
                Rule::identifier => Ok(Expr::Variable(primary_pair.as_str().to_string())),
                Rule::call => {
                    let mut call_inner = primary_pair.into_inner();
                    let callee = Box::new(Expr::Variable(call_inner.next().unwrap().as_str().to_string()));
                    let mut args = Vec::new();
                    let args_pair_opt = call_inner.next();
                    if let Some(args_pair) = args_pair_opt {
                        for arg in args_pair.into_inner() {
                            args.push(build_expr(arg)?);
                        }
                    }
                    Ok(Expr::Call(callee, args))
                }
                Rule::expression => build_expr(primary_pair), // for paren
                _ => Err(format!("Unexpected primary: {:?}", primary_pair)),
            }
        }
        _ => Err(format!("Unexpected rule in expr: {:?}", pair.as_rule())),
    }
}

// Basic semantic analysis: check variable definitions, simple type inference/check
fn analyze(stmts: &[Stmt], scope: &mut Scope, scopes: &mut Vec<Scope>) -> Result<(), String> {
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl(name, expr) => {
                let typ = infer_type(expr, scope)?;
                if scope.types.contains_key(name) {
                    return Err(format!("Variable {} already defined", name));
                }
                scope.types.insert(name.clone(), typ);
            }
            Stmt::ExprStmt(expr) => {
                infer_type(expr, scope)?;
            }
            Stmt::Function(name, params, body, ret_type) => {
                if scope.types.contains_key(name) {
                    return Err(format!("Function {} already defined", name));
                }
                let mut param_types = Vec::new();
                let mut func_scope = Scope { types: HashMap::new() };
                for param in params {
                    func_scope.types.insert(param.clone(), Type::Any); // Assume Any for now
                    param_types.push(Type::Any);
                }
                scopes.push(func_scope);
                analyze(body, scopes.last_mut().unwrap(), scopes)?;
                let _ = scopes.pop();
                let func_type = Type::Function(param_types, Box::new(Type::Any));
                scope.types.insert(name.clone(), func_type);
            }
            Stmt::Return(expr) => {
                infer_type(expr, scope)?;
                // Check against function return type if available
            }
            Stmt::Require(_) => {
                // TODO: Load module and merge scopes
            }
            Stmt::Embedded(_, _) => {
                // TODO: Depending on lang, analyze or skip
            }
        }
    }
    Ok(())
}

fn infer_type(expr: &Expr, scope: &Scope) -> Result<Type, String> {
    match expr {
        Expr::Literal(lit) => Ok(match lit {
            Literal::Number(_) => Type::Number,
            Literal::String(_) => Type::String,
        }),
        Expr::Variable(name) => scope.types.get(name).cloned().ok_or_else(|| format!("Undefined variable: {}", name)),
        Expr::Binary(left, op, right) => {
            let left_ty = infer_type(left, scope)?;
            let right_ty = infer_type(right, scope)?;
            if left_ty != Type::Number || right_ty != Type::Number {
                return Err("Binary ops require numbers".to_string());
            }
            match op {
                Op::Plus | Op::Minus | Op::Star | Op::Slash => Ok(Type::Number),
            }
        }
        Expr::Assign(name, right) => {
            let right_ty = infer_type(right, scope)?;
            if let Some(existing_ty) = scope.types.get(name) {
                if *existing_ty != right_ty {
                    return Err(format!("Type mismatch for assignment to {}", name));
                }
            }
            Ok(right_ty)
        }
        Expr::Call(callee, args) => {
            let callee_ty = infer_type(callee, scope)?;
            if let Type::Function(param_types, ret_type) = callee_ty {
                if param_types.len() != args.len() {
                    return Err("Arity mismatch".to_string());
                }
                for (param_ty, arg) in param_types.iter().zip(args.iter()) {
                    let arg_ty = infer_type(arg, scope)?;
                    if *param_ty != arg_ty && *param_ty != Type::Any && arg_ty != Type::Any {
                        return Err("Argument type mismatch".to_string());
                    }
                }
                Ok(*ret_type)
            } else {
                Err("Not callable".to_string())
            }
        }
    }
}
