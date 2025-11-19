// src/main.rs
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::Path;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use cranelift::prelude::*;
use cranelift_codegen::isa::{self, CallConv};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

// TokenType enum
#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Identifier,
    Number,
    String,
    KeywordFunction,
    KeywordReturn,
    KeywordRequire,
    LBracket,  // [
    RBracket,  // ]
    LParen,    // (
    RParen,    // )
    Colon,     // :
    Comma,     // ,
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Equals,    // =
    Hash,      // #
    Tilde,     // ~
    ColonColon,// ::
    Eof,
    Unknown,
}

// Token struct
#[derive(Debug, Clone)]
struct Token {
    typ: TokenType,
    lexeme: String,
    line: usize,
}

// Lexer struct
struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        let chars: Vec<char> = source.chars().collect();
        Self {
            source,
            chars,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        match c {
            '[' => self.make_token(TokenType::LBracket),
            ']' => self.make_token(TokenType::RBracket),
            '(' => self.make_token(TokenType::LParen),
            ')' => self.make_token(TokenType::RParen),
            ':' => {
                if self.match_char(':') {
                    self.make_token(TokenType::ColonColon)
                } else {
                    self.make_token(TokenType::Colon)
                }
            }
            ',' => self.make_token(TokenType::Comma),
            '+' => self.make_token(TokenType::Plus),
            '-' => self.make_token(TokenType::Minus),
            '*' => self.make_token(TokenType::Star),
            '/' => self.make_token(TokenType::Slash),
            '=' => self.make_token(TokenType::Equals),
            '#' => self.make_token(TokenType::Hash),
            '~' => self.make_token(TokenType::Tilde),
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        c
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.chars[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => { let _ = self.advance(); }
                '\n' => {
                    self.line += 1;
                    let _ = self.advance();
                }
                '~' => self.skip_single_line_comment(),
                ':' => {
                    if self.match_char(':') {
                        self.skip_multi_line_comment();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn skip_single_line_comment(&mut self) {
        while self.peek() != '\n' && !self.is_at_end() {
            let _ = self.advance();
        }
    }

    fn skip_multi_line_comment(&mut self) {
        loop {
            if self.is_at_end() {
                return;
            }
            if self.peek() == ':' && self.match_char(':') {
                return;
            }
            if self.advance() == '\n' {
                self.line += 1;
            }
        }
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            let _ = self.advance();
        }
        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }
        let _ = self.advance(); // closing "
        self.make_token(TokenType::String)
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            let _ = self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            let _ = self.advance();
            while self.peek().is_ascii_digit() {
                let _ = self.advance();
            }
        }
        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            let _ = self.advance();
        }
        let typ = self.identifier_type();
        self.make_token(typ)
    }

    fn identifier_type(&self) -> TokenType {
        let text = &self.source[self.start..self.current];
        match text {
            "function" => TokenType::KeywordFunction,
            "return" => TokenType::KeywordReturn,
            "require" => TokenType::KeywordRequire,
            _ => TokenType::Identifier,
        }
    }

    fn make_token(&self, typ: TokenType) -> Token {
        Token {
            typ,
            lexeme: self.source[self.start..self.current].to_string(),
            line: self.line,
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            typ: TokenType::Unknown,
            lexeme: message.to_string(),
            line: self.line,
        }
    }
}

// AST definitions
#[derive(Debug, Clone)]
enum Expr {
    Literal { value: String },
    Binary { left: Box<Expr>, op: Token, right: Box<Expr> },
    Variable { name: Token },
    Call { callee: Box<Expr>, args: Vec<Expr> },
}

#[derive(Debug, Clone)]
enum Stmt {
    ExprStmt(Expr),
    Function { name: Token, params: Vec<Token>, body: Vec<Stmt> },
    Return(Expr),
    Require(String),
    Embedded { lang: String, code: String },
    VarDecl { name: Token, initializer: Expr },
}

// Parser struct
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self.match_type(TokenType::Identifier) && self.match_type(TokenType::Equals) {
            let name = self.previous(1).clone();
            let init = self.expression()?;
            return Ok(Stmt::VarDecl { name, initializer: init });
        }
        self.statement()
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.match_type(TokenType::KeywordFunction) {
            return self.function();
        }
        if self.match_type(TokenType::KeywordReturn) {
            return Ok(Stmt::Return(self.expression()?));
        }
        if self.match_type(TokenType::KeywordRequire) {
            let str_token = self.consume(TokenType::String, "Expect string after require.")?;
            return Ok(Stmt::Require(str_token.lexeme));
        }
        if self.match_type(TokenType::Hash) {
            return self.embedded();
        }
        Ok(Stmt::ExprStmt(self.expression()?))
    }

    fn function(&mut self) -> Result<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expect function name.")?;
        self.consume(TokenType::LParen, "Expect '(' after function name.")?;
        let mut params = Vec::new();
        if !self.check(TokenType::RParen) {
            loop {
                params.push(self.consume(TokenType::Identifier, "Expect parameter name.")?);
                if !self.match_type(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::LBracket, "Expect '[' before function body.")?;
        let mut body = Vec::new();
        while !self.check(TokenType::RBracket) && !self.is_at_end() {
            body.push(self.declaration()?);
        }
        self.consume(TokenType::RBracket, "Expect ']' after function body.")?;
        Ok(Stmt::Function { name, params, body })
    }

    fn embedded(&mut self) -> Result<Stmt> {
        let mut lang = String::new();
        while !self.match_type(TokenType::Equals) {
            lang.push_str(&self.advance().lexeme);
        }
        self.consume(TokenType::LBracket, "Expect '[' after language name.")?;
        let mut code = String::new();
        let mut depth = 1;
        while depth > 0 && !self.is_at_end() {
            let tok = self.advance();
            code.push_str(&tok.lexeme);
            match tok.typ {
                TokenType::LBracket => depth += 1,
                TokenType::RBracket => depth -= 1,
                _ => {},
            }
        }
        if depth > 0 {
            return Err(anyhow::anyhow!("Unterminated embedded code."));
        }
        Ok(Stmt::Embedded { lang, code })
    }

    fn expression(&mut self) -> Result<Expr> {
        self.additive()
    }

    fn additive(&mut self) -> Result<Expr> {
        let mut expr = self.multiplicative()?;
        while self.match_type(TokenType::Plus) || self.match_type(TokenType::Minus) {
            let op = self.previous().clone();
            let right = self.multiplicative()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }
        Ok(expr)
    }

    fn multiplicative(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        while self.match_type(TokenType::Star) || self.match_type(TokenType::Slash) {
            let op = self.previous().clone();
            let right = self.primary()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr> {
        if self.match_type(TokenType::Number) || self.match_type(TokenType::String) {
            return Ok(Expr::Literal { value: self.previous().lexeme.clone() });
        }
        if self.match_type(TokenType::Identifier) {
            if self.match_type(TokenType::LParen) {
                let mut args = Vec::new();
                if !self.check(TokenType::RParen) {
                    loop {
                        args.push(self.expression()?);
                        if !self.match_type(TokenType::Comma) {
                            break;
                        }
                    }
                }
                self.consume(TokenType::RParen, "Expect ')' after arguments.")?;
                let callee = Box::new(Expr::Variable { name: self.tokens[self.current - 2].clone() });
                return Ok(Expr::Call { callee, args });
            }
            return Ok(Expr::Variable { name: self.previous().clone() });
        }
        Err(anyhow::anyhow!("Expect expression."))
    }

    fn match_type(&mut self, typ: TokenType) -> bool {
        if self.check(typ) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, typ: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().typ == typ
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().clone()
    }

    fn is_at_end(&self) -> bool {
        self.peek().typ == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn previous_n(&self, n: usize) -> &Token {
        &self.tokens[self.current - n]
    }

    fn consume(&mut self, typ: TokenType, message: &str) -> Result<Token> {
        if self.check(typ) {
            Ok(self.advance())
        } else {
            Err(anyhow::anyhow!("{} at line {}", message, self.peek().line))
        }
    }
}

// Codegen
struct CodeGen<'a> {
    module: &'a mut ObjectModule,
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    variables: HashMap<String, Variable>,
    var_index: usize,
    functions: HashMap<String, FuncId>,
}

impl<'a> CodeGen<'a> {
    fn new(module: &'a mut ObjectModule) -> Self {
        Self {
            module,
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            variables: HashMap::new(),
            var_index: 0,
            functions: HashMap::new(),
        }
    }

    fn compile(&mut self, ast: &Vec<Stmt>) -> Result<()> {
        // Declare main if not present
        let mut has_main = false;
        for stmt in ast {
            if let Stmt::Function { name, .. } = stmt {
                if name.lexeme == "main" {
                    has_main = true;
                    break;
                }
            }
        }

        if !has_main {
            return Err(anyhow::anyhow!("No main function found."));
        }

        // Predeclare functions
        for stmt in ast {
            if let Stmt::Function { name, params, .. } = stmt {
                let mut sig = self.module.make_signature();
                for _ in params {
                    sig.params.push(AbiParam::new(types::I32)); // Assume i32 for simplicity
                }
                sig.returns.push(AbiParam::new(types::I32));
                let func_id = self.module.declare_function(&name.lexeme, Linkage::Export, &sig)?;
                self.functions.insert(name.lexeme.clone(), func_id);
            }
        }

        // Compile functions
        for stmt in ast {
            if let Stmt::Function { name, params, body } = stmt {
                self.compile_function(&name.lexeme, params, body)?;
            }
        }

        Ok(())
    }

    fn compile_function(&mut self, name: &str, params: &Vec<Token>, body: &Vec<Stmt>) -> Result<()> {
        self.ctx.func.signature.clear();
        for _ in params {
            self.ctx.func.signature.params.push(AbiParam::new(types::I32));
        }
        self.ctx.func.signature.returns.push(AbiParam::new(types::I32));

        self.ctx.func.name = ExternalName::user(0, 0); // Dummy

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        self.variables.clear();
        self.var_index = 0;

        // Params as variables
        let params_values = builder.block_params(entry_block).to_vec();
        for (i, param) in params.iter().enumerate() {
            let var = self.declare_variable(types::I32);
            builder.def_var(var, params_values[i]);
            self.variables.insert(param.lexeme.clone(), var);
        }

        for stmt in body {
            self.codegen_stmt(stmt, &mut builder)?;
        }

        // Default return 0 if no return
        let zero = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[zero]);

        builder.finalize();

        let func_id = *self.functions.get(name).unwrap();
        self.module.define_function(func_id, &mut self.ctx)?;

        self.module.clear_context(&mut self.ctx);

        Ok(())
    }

    fn codegen_stmt(&mut self, stmt: &Stmt, builder: &mut FunctionBuilder) -> Result<()> {
        match stmt {
            Stmt::ExprStmt(expr) => {
                let _ = self.codegen_expr(expr, builder)?;
            }
            Stmt::Return(expr) => {
                let val = self.codegen_expr(expr, builder)?;
                builder.ins().return_(&[val]);
            }
            Stmt::VarDecl { name, initializer } => {
                let val = self.codegen_expr(initializer, builder)?;
                let var = self.declare_variable(types::I32); // Assume i32
                builder.def_var(var, val);
                self.variables.insert(name.lexeme.clone(), var);
            }
            _ => {} // Ignore others for simplicity
        }
        Ok(())
    }

    fn codegen_expr(&mut self, expr: &Expr, builder: &mut FunctionBuilder) -> Result<Value> {
        match expr {
            Expr::Literal { value } => {
                let num = value.parse::<i64>().unwrap_or(0);
                Ok(builder.ins().iconst(types::I32, num))
            }
            Expr::Variable { name } => {
                if let Some(var) = self.variables.get(&name.lexeme) {
                    Ok(builder.use_var(*var))
                } else {
                    Err(anyhow::anyhow!("Undefined variable '{}'.", name.lexeme))
                }
            }
            Expr::Binary { left, op, right } => {
                let lhs = self.codegen_expr(left, builder)?;
                let rhs = self.codegen_expr(right, builder)?;
                match op.typ {
                    TokenType::Plus => Ok(builder.ins().iadd(lhs, rhs)),
                    TokenType::Minus => Ok(builder.ins().isub(lhs, rhs)),
                    TokenType::Star => Ok(builder.ins().imul(lhs, rhs)),
                    TokenType::Slash => Ok(builder.ins().sdiv(lhs, rhs)),
                    _ => Err(anyhow::anyhow!("Invalid operator.")),
                }
            }
            Expr::Call { callee, args } => {
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.codegen_expr(arg, builder)?);
                }
                if let Expr::Variable { name } = **callee {
                    if let Some(func_id) = self.functions.get(&name.lexeme) {
                        let local_callee = self.module.declare_func_in_func(*func_id, builder.func);
                        let call = builder.ins().call(local_callee, &arg_vals);
                        Ok(builder.inst_results(call)[0])
                    } else {
                        Err(anyhow::anyhow!("Undefined function '{}'.", name.lexeme))
                    }
                } else {
                    Err(anyhow::anyhow!("Callee not a variable."))
                }
            }
        }
    }

    fn declare_variable(&mut self, typ: Type) -> Variable {
        let var = Variable::new(self.var_index);
        self.ctx.func.dfg.block_params(builder.current_block().unwrap()).iter(); // Dummy to ensure
        self.ctx.func.dfg.append_var_def(var, typ);
        self.var_index += 1;
        var
    }
}

// CLI
#[derive(Parser)]
#[command(name = "compiler")]
#[command(about = "Vib Compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        input: String,
        output: Option<String>,
    },
    // Add clean etc. later
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { input, output } => {
            let source = fs::read_to_string(&input).context("Failed to read input file")?;
            let mut lexer = Lexer::new(&source);
            let mut tokens = Vec::new();
            loop {
                let token = lexer.next_token();
                tokens.push(token.clone());
                if token.typ == TokenType::Eof {
                    break;
                }
            }

            let mut parser = Parser::new(tokens);
            let ast = parser.parse()?;

            let flags = settings::builder();
            let isa_builder = isa::lookup(Triple::host())?;
            let isa = isa_builder.finish(settings::Flags::new(flags))?;

            let builder = ObjectBuilder::new(isa, "vib_module", cranelift_module::default_libcall_names())?;
            let mut module = ObjectModule::new(builder);

            let mut codegen = CodeGen::new(&mut module);
            codegen.compile(&ast)?;

            let obj = module.finish();

            let output_path = output.unwrap_or_else(|| "a.out".to_string());
            let mut file = File::create(&output_path)?;
            file.write_all(&obj.emit()?)?;

            println!("Compiled to {}", output_path);
        }
    }

    Ok(())
}
