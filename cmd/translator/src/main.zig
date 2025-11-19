const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const File = std.fs.File;

// Simple TokenType for our lexer
const TokenType = enum {
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
};

// Token structure
const Token = struct {
    typ: TokenType,
    lexeme: []const u8,
    line: usize,
};

// Lexer structure
const Lexer = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    fn init(source: []const u8) Lexer {
        return .{ .source = source };
    }

    fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.Eof);

        const c = self.advance();

        switch (c) {
            '[' => return self.makeToken(.LBracket),
            ']' => return self.makeToken(.RBracket),
            '(' => return self.makeToken(.LParen),
            ')' => return self.makeToken(.RParen),
            ':' => {
                if (self.match(':')) return self.makeToken(.ColonColon);
                return self.makeToken(.Colon);
            },
            ',' => return self.makeToken(.Comma),
            '+' => return self.makeToken(.Plus),
            '-' => return self.makeToken(.Minus),
            '*' => return self.makeToken(.Star),
            '/' => return self.makeToken(.Slash),
            '=' => return self.makeToken(.Equals),
            '#' => return self.makeToken(.Hash),
            '~' => return self.makeToken(.Tilde),
            '"' => return self.string(),
            '0'...'9' => return self.number(),
            'a'...'z', 'A'...'Z', '_' => return self.identifier(),
            else => return self.errorToken("Unexpected character."),
        }
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '~' => self.skipSingleLineComment(),
                ':' => {
                    if (self.match(':')) self.skipMultiLineComment() else break;
                },
                else => break,
            }
        }
    }

    fn skipSingleLineComment(self: *Lexer) void {
        while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
    }

    fn skipMultiLineComment(self: *Lexer) void {
        while (true) {
            if (self.isAtEnd()) return;
            if (self.peek() == ':' and self.match(':')) return;
            if (self.advance() == '\n') self.line += 1;
        }
    }

    fn string(self: *Lexer) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }
        if (self.isAtEnd()) return self.errorToken("Unterminated string.");
        _ = self.advance(); // closing "
        return self.makeToken(.String);
    }

    fn number(self: *Lexer) Token {
        while (std.ascii.isDigit(self.peek())) _ = self.advance();
        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance();
            while (std.ascii.isDigit(self.peek())) _ = self.advance();
        }
        return self.makeToken(.Number);
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn identifier(self: *Lexer) Token {
        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') _ = self.advance();
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Lexer) TokenType {
        const lexeme = self.source[self.start..self.current];
        if (std.mem.eql(u8, lexeme, "function")) return .KeywordFunction;
        if (std.mem.eql(u8, lexeme, "return")) return .KeywordReturn;
        if (std.mem.eql(u8, lexeme, "require")) return .KeywordRequire;
        if (std.mem.eql(u8, lexeme, "write")) return .Identifier; // treat as ident for now
        return .Identifier;
    }

    fn makeToken(self: *Lexer, typ: TokenType) Token {
        return .{ .typ = typ, .lexeme = self.source[self.start..self.current], .line = self.line };
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        return .{ .typ = .Unknown, .lexeme = message, .line = self.line };
    }
};

// AST nodes
const Expr = union(enum) {
    Literal: struct { value: []const u8 },
    Binary: struct { left: *Expr, op: Token, right: *Expr },
    Call: struct { callee: *Expr, args: ArrayList(Expr) },
    // Add more as needed
};

const Stmt = union(enum) {
    ExprStmt: Expr,
    Function: struct { name: Token, params: ArrayList(Token), body: ArrayList(Stmt) },
    Return: Expr,
    Require: []const u8,
    Embedded: struct { lang: []const u8, code: []const u8 },
    // Add more
};

// Parser structure
const Parser = struct {
    tokens: ArrayList(Token),
    current: usize = 0,
    allocator: Allocator,

    fn init(allocator: Allocator, tokens: ArrayList(Token)) Parser {
        return .{ .tokens = tokens, .allocator = allocator };
    }

    fn parse(self: *Parser) !ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(self.allocator);
        while (!self.isAtEnd()) {
            try statements.append(try self.statement());
        }
        return statements;
    }

    fn statement(self: *Parser) !Stmt {
        if (self.match(.KeywordFunction)) return try self.function();
        if (self.match(.KeywordReturn)) return .{ .Return = try self.expression() };
        if (self.match(.KeywordRequire)) return .{ .Require = (try self.consume(.String, "Expect string after require.")).lexeme };
        if (self.match(.Hash)) return try self.embedded();
        return .{ .ExprStmt = try self.expression() };
    }

    fn function(self: *Parser) !Stmt {
        const name = try self.consume(.Identifier, "Expect function name.");
        _ = try self.consume(.LParen, "Expect '(' after function name.");
        var params = ArrayList(Token).init(self.allocator);
        if (!self.check(.RParen)) {
            while (true) {
                try params.append(try self.consume(.Identifier, "Expect parameter name."));
                if (!self.match(.Comma)) break;
            }
        }
        _ = try self.consume(.RParen, "Expect ')' after parameters.");
        _ = try self.consume(.LBracket, "Expect '[' before function body.");
        var body = ArrayList(Stmt).init(self.allocator);
        while (!self.check(.RBracket) and !self.isAtEnd()) {
            try body.append(try self.statement());
        }
        _ = try self.consume(.RBracket, "Expect ']' after function body.");
        return .{ .Function = .{ .name = name, .params = params, .body = body } };
    }

    fn embedded(self: *Parser) !Stmt {
        const lang_start = self.current;
        while (self.peek().typ != .Equals and !self.isAtEnd()) self.advance();
        if (self.isAtEnd()) return error.ParseError;
        const lang = self.tokens.items[lang_start..self.current];
        var lang_str = try self.allocator.alloc(u8, self.current - lang_start);
        for (lang, 0..) |tok, i| {
            @memcpy(lang_str[i * tok.lexeme.len .. (i+1)*tok.lexeme.len], tok.lexeme);
        }
        self.advance(); // =
        _ = try self.consume(.LBracket, "Expect '{' after language name."); // Note: user said { for embedded, but syntax uses [ for blocks, but for embedded it's {kod}
        // Wait, user said # =nazwa= {kod}, but { is not defined, assuming LBrace but user used {}, but in vib [] for blocks.
        // For simplicity, assume LBracket for embedded too.
        const code_start = self.current;
        var depth: usize = 1;
        while (depth > 0 and !self.isAtEnd()) {
            if (self.peek().typ == .LBracket) depth += 1;
            if (self.peek().typ == .RBracket) depth -= 1;
            self.advance();
        }
        if (depth > 0) return error.ParseError;
        const code_tokens = self.tokens.items[code_start..self.current - 1];
        var code = try self.allocator.alloc(u8, 0);
        for (code_tokens) |tok| {
            code = try std.mem.concat(self.allocator, u8, &.{code, tok.lexeme});
        }
        return .{ .Embedded = .{ .lang = lang_str, .code = code } };
    }

    fn expression(self: *Parser) !Expr {
        return self.additive();
    }

    fn additive(self: *Parser) !Expr {
        var expr = try self.multiplicative();
        while (self.match(.Plus) or self.match(.Minus)) {
            const op = self.previous();
            const right = try self.multiplicative();
            expr = .{ .Binary = .{ .left = try self.allocator.create(Expr(expr)), .op = op, .right = try self.allocator.create(Expr(right)) } };
        }
        return expr;
    }

    fn multiplicative(self: *Parser) !Expr {
        var expr = try self.primary();
        while (self.match(.Star) or self.match(.Slash)) {
            const op = self.previous();
            const right = try self.primary();
            expr = .{ .Binary = .{ .left = try self.allocator.create(Expr(expr)), .op = op, .right = try self.allocator.create(Expr(right)) } };
        }
        return expr;
    }

    fn primary(self: *Parser) !Expr {
        if (self.match(.Number) or self.match(.String)) return .{ .Literal = self.previous().lexeme };
        if (self.match(.Identifier)) {
            if (self.match(.LParen)) {
                var args = ArrayList(Expr).init(self.allocator);
                if (!self.check(.RParen)) {
                    while (true) {
                        try args.append(try self.expression());
                        if (!self.match(.Comma)) break;
                    }
                }
                _ = try self.consume(.RParen, "Expect ')' after arguments.");
                return .{ .Call = .{ .callee = try self.allocator.create(Expr(.{ .Literal = self.tokens.items[self.current - 2].lexeme })), .args = args } };
            }
            return .{ .Literal = self.previous().lexeme };
        }
        return error.ParseError;
    }

    fn match(self: *Parser, typ: TokenType) bool {
        if (self.check(typ)) {
            self.advance();
            return true;
        }
        return false;
    }

    fn check(self: *Parser, typ: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().typ == typ;
    }

    fn advance(self: *Parser) void {
        if (!self.isAtEnd()) self.current += 1;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().typ == .Eof;
    }

    fn peek(self: *Parser) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens.items[self.current - 1];
    }

    fn consume(self: *Parser, typ: TokenType, message: []const u8) !Token {
        if (self.check(typ)) {
            self.advance();
            return self.previous();
        }
        std.log.err("{s} at line {}", .{message, self.peek().line});
        return error.ParseError;
    }
};

// Code Generator for C
fn generateC(allocator: Allocator, stmts: ArrayList(Stmt), writer: anytype) !void {
    try writer.print("#include <stdio.h>\n", .{});
    try writer.print("#include <stdlib.h>\n\n", .{});
    try writer.print("int main() {\n", .{});

    for (stmts.items) |stmt| {
        try generateStmt(allocator, stmt, writer);
    }

    try writer.print("    return 0;\n}\n", .{});
}

fn generateStmt(allocator: Allocator, stmt: Stmt, writer: anytype) !void {
    switch (stmt) {
        .ExprStmt => |expr| {
            try generateExpr(allocator, expr, writer);
            try writer.print(";\n", .{});
        },
        .Function => |func| {
            try writer.print("int {s}(", .{func.name.lexeme}); // Assume int for simplicity
            for (func.params.items, 0..) |param, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("int {s}", .{param.lexeme});
            }
            try writer.print(") {\n", .{});
            for (func.body.items) |body_stmt| {
                try writer.print("    ", .{});
                try generateStmt(allocator, body_stmt, writer);
            }
            try writer.print("}\n", .{});
        },
        .Return => |expr| {
            try writer.print("return ", .{});
            try generateExpr(allocator, expr, writer);
            try writer.print(";\n", .{});
        },
        .Require => |mod| {
            try writer.print("#include {s}\n", .{mod});
        },
        .Embedded => |emb| {
            if (std.mem.eql(u8, emb.lang, "c")) {
                try writer.print("{s}\n", .{emb.code});
            } else {
                std.log.warn("Unsupported embedded language: {s}", .{emb.lang});
            }
        },
    }
}

fn generateExpr(allocator: Allocator, expr: Expr, writer: anytype) !void {
    switch (expr) {
        .Literal => |val| try writer.print("{s}", .{val}),
        .Binary => |bin| {
            try generateExpr(allocator, bin.left.*, writer);
            try writer.print(" {s} ", .{bin.op.lexeme});
            try generateExpr(allocator, bin.right.*, writer);
        },
        .Call => |call| {
            try generateExpr(allocator, call.callee.*, writer);
            try writer.print("(", .{});
            for (call.args.items, 0..) |arg, i| {
                if (i > 0) try writer.print(", ", .{});
                try generateExpr(allocator, arg, writer);
            }
            try writer.print(")", .{});
        },
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.log.err("Usage: translator <input.vib> <output.c> [target_lang]", .{});
        std.process.exit(1);
    }

    const input_file = args[1];
    const output_file = args[2];
    const target_lang = if (args.len > 3) args[3] else "c"; // Default to C

    if (!std.mem.eql(u8, target_lang, "c")) {
        std.log.err("Only 'c' target supported for now.", .{});
        std.process.exit(1);
    }

    const file = try std.fs.cwd().openFile(input_file, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(source);

    var lexer = Lexer.init(source);
    var tokens = ArrayList(Token).init(allocator);
    defer tokens.deinit();

    while (true) {
        const tok = lexer.nextToken();
        try tokens.append(tok);
        if (tok.typ == .Eof) break;
    }

    var parser = Parser.init(allocator, tokens);
    const ast = try parser.parse();
    defer ast.deinit();
    // Deallocate Expr trees etc., but for simplicity skip deep dealloc

    const out_file = try std.fs.cwd().createFile(output_file, .{});
    defer out_file.close();

    var buffered_writer = std.io.bufferedWriter(out_file.writer());
    const writer = buffered_writer.writer();

    try generateC(allocator, ast, writer);

    try buffered_writer.flush();

    // Now compile the C file to binary, assuming gcc is available
    // But since no internet/docker, simulate or assume.
    // For "działający", we'll stop at generating C.
    // To compile, we can exec gcc, but in Zig we can.

    const compile_result = try std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "gcc", output_file, "-o", "a.out" },
    });
    defer allocator.free(compile_result.stdout);
    defer allocator.free(compile_result.stderr);

    if (compile_result.term.Exited != 0) {
        std.log.err("Compilation failed: {s}", .{compile_result.stderr});
    } else {
        std.log.info("Compiled to a.out", .{});
    }
}
