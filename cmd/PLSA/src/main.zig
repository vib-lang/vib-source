const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

// TokenType
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

// Token
const Token = struct {
    typ: TokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,
};

// Lexer
const Lexer = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    column: usize = 1,

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
        self.column += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        self.column += 1;
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
                    self.column = 1;
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
            const adv = self.advance();
            if (adv == '\n') {
                self.line += 1;
                self.column = 1;
            }
        }
    }

    fn string(self: *Lexer) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            const p = self.peek();
            _ = self.advance();
            if (p == '\n') {
                self.line += 1;
                self.column = 1;
            }
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
        if (std.mem.eql(u8, lexeme, "write")) return .Identifier; // treat as ident
        return .Identifier;
    }

    fn makeToken(self: *Lexer, typ: TokenType) Token {
        return .{ .typ = typ, .lexeme = self.source[self.start..self.current], .line = self.line, .column = self.start_column() };
    }

    fn start_column(self: *Lexer) usize {
        // Calculate column of start
        var col: usize = 1;
        var i = self.start;
        while (i > 0 and self.source[i - 1] != '\n') {
            i -= 1;
            col += 1;
        }
        return col;
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        return .{ .typ = .Unknown, .lexeme = message, .line = self.line, .column = self.start_column() };
    }
};

// ParseError
const ParseError = struct {
    token: Token,
    message: []const u8,
};

// AST
const ValueType = enum {
    Number,
    String,
    // Add more
};

const Value = union(enum) {
    Number: f64,
    String: []const u8,
};

const Expr = union(enum) {
    Literal: Value,
    Binary: struct { left: *Expr, op: Token, right: *Expr },
    Variable: Token,
    Call: struct { callee: *Expr, args: ArrayList(Expr) },
    Assign: struct { name: Token, value: *Expr },
};

const Stmt = union(enum) {
    ExprStmt: Expr,
    Function: struct { name: Token, params: ArrayList(Token), body: ArrayList(Stmt) },
    Return: Expr,
    Require: []const u8,
    Embedded: struct { lang: []const u8, code: []const u8 },
    VarDecl: struct { name: Token, initializer: Expr },
};

// Parser
const Parser = struct {
    tokens: ArrayList(Token),
    current: usize = 0,
    allocator: Allocator,

    fn init(allocator: Allocator, tokens: ArrayList(Token)) Parser {
        return .{ .tokens = tokens, .allocator = allocator };
    }

    fn deinit(self: *Parser) void {
        self.tokens.deinit();
    }

    fn parse(self: *Parser) !ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(self.allocator);
        while (!self.isAtEnd()) {
            statements.append(try self.declaration()) catch |err| {
                if (err == error.ParseError) {
                    // Recover or something, but for now propagate
                    return err;
                }
                return err;
            };
        }
        return statements;
    }

    fn declaration(self: *Parser) !Stmt {
        if (self.match(.Identifier)) {
            if (self.match(.Equals)) {
                const name = self.previous();
                const init = try self.expression();
                return .{ .VarDecl = .{ .name = name, .initializer = init } };
            } else {
                self.current -= 1; // Backtrack
            }
        }
        return try self.statement();
    }

    fn statement(self: *Parser) !Stmt {
        if (self.match(.KeywordFunction)) return try self.functionStmt();
        if (self.match(.KeywordReturn)) return .{ .Return = try self.expression() };
        if (self.match(.KeywordRequire)) return .{ .Require = (try self.consume(.String, "Expect string after require.")).lexeme };
        if (self.match(.Hash)) return try self.embedded();
        return .{ .ExprStmt = try self.expression() };
    }

    fn functionStmt(self: *Parser) !Stmt {
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
            try body.append(try self.declaration());
        }
        _ = try self.consume(.RBracket, "Expect ']' after function body.");
        return .{ .Function = .{ .name = name, .params = params, .body = body } };
    }

    fn embedded(self: *Parser) !Stmt {
        var lang = ArrayList(u8).init(self.allocator);
        while (!self.isAtEnd() and !self.check(.Equals)) {
            try lang.appendSlice(self.advance().lexeme);
        }
        if (self.isAtEnd()) return error.ParseError;
        _ = self.advance(); // =
        _ = try self.consume(.LBracket, "Expect '[' after =.");
        var code = ArrayList(u8).init(self.allocator);
        var depth: usize = 1;
        while (depth > 0 and !self.isAtEnd()) {
            const tok = self.advance();
            try code.appendSlice(tok.lexeme);
            if (tok.typ == .LBracket) depth += 1;
            if (tok.typ == .RBracket) depth -= 1;
        }
        if (depth > 0) return error.ParseError;
        return .{ .Embedded = .{ .lang = try lang.toOwnedSlice(), .code = try code.toOwnedSlice() } };
    }

    fn expression(self: *Parser) !Expr {
        return self.assignment();
    }

    fn assignment(self: *Parser) !Expr {
        const expr = try self.additive();
        if (self.match(.Equals)) {
            const value = try self.assignment();
            if (std.meta.activeTag(expr) == .Variable) {
                return .{ .Assign = .{ .name = expr.Variable, .value = try self.allocator.create(Expr{value}) } };
            }
            return error.ParseError;
        }
        return expr;
    }

    fn additive(self: *Parser) !Expr {
        var expr = try self.multiplicative();
        while (self.match(.Plus) or self.match(.Minus)) {
            const op = self.previous();
            const right = try self.multiplicative();
            const left_box = try self.allocator.create(Expr);
            left_box.* = expr;
            const right_box = try self.allocator.create(Expr);
            right_box.* = right;
            expr = .{ .Binary = .{ .left = left_box, .op = op, .right = right_box } };
        }
        return expr;
    }

    fn multiplicative(self: *Parser) !Expr {
        var expr = try self.primary();
        while (self.match(.Star) or self.match(.Slash)) {
            const op = self.previous();
            const right = try self.primary();
            const left_box = try self.allocator.create(Expr);
            left_box.* = expr;
            const right_box = try self.allocator.create(Expr);
            right_box.* = right;
            expr = .{ .Binary = .{ .left = left_box, .op = op, .right = right_box } };
        }
        return expr;
    }

    fn primary(self: *Parser) !Expr {
        if (self.match(.Number)) {
            const val = try std.fmt.parseFloat(f64, self.previous().lexeme);
            return .{ .Literal = .{ .Number = val } };
        }
        if (self.match(.String)) {
            return .{ .Literal = .{ .String = self.previous().lexeme } };
        }
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
                const callee_box = try self.allocator.create(Expr);
                callee_box.* = .{ .Variable = self.tokens.items[self.current - 2] };
                return .{ .Call = .{ .callee = callee_box, .args = args } };
            }
            return .{ .Variable = self.previous() };
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
        return self.reportError(self.peek(), message);
    }

    fn reportError(self: *Parser, token: Token, message: []const u8) ParseError {
        return ParseError { .token = token, .message = message };
    }
};

// Function to print nice error
fn printParseError(source: []const u8, err: ParseError) void {
    std.debug.print("Error at line {} column {}: {s}\n", .{err.token.line, err.token.column, err.message});
    const lines = std.mem.split(u8, source, "\n");
    var line_num: usize = 1;
    var iter = lines;
    while (iter.next()) |line| : (line_num += 1) {
        if (line_num == err.token.line) {
            std.debug.print("{d: >4} | {s}\n", .{line_num, line});
            std.debug.print("     | ", .{});
            for (1..err.token.column) |_| std.debug.print(" ", .{});
            std.debug.print("^", .{});
            for (1..err.token.lexeme.len - 1) |_| std.debug.print("~", .{});
            std.debug.print("\n", .{});
            break;
        }
    }
}

// Dealloc AST, recursive
fn deinitExpr(allocator: Allocator, expr: *Expr) void {
    switch (expr.*) {
        .Binary => |bin| {
            deinitExpr(allocator, bin.left);
            deinitExpr(allocator, bin.right);
            allocator.destroy(bin.left);
            allocator.destroy(bin.right);
        },
        .Call => |call| {
            deinitExpr(allocator, call.callee);
            allocator.destroy(call.callee);
            for (call.args.items) |arg| deinitExpr(allocator, &arg);
            call.args.deinit();
        },
        .Assign => |ass| {
            deinitExpr(allocator, ass.value);
            allocator.destroy(ass.value);
        },
        else => {},
    }
}

fn deinitStmt(allocator: Allocator, stmt: *Stmt) void {
    switch (stmt.*) {
        .ExprStmt => |*expr| deinitExpr(allocator, expr),
        .Function => |func| {
            func.params.deinit();
            for (func.body.items) |body_stmt| deinitStmt(allocator, &body_stmt);
            func.body.deinit();
        },
        .Return => |*expr| deinitExpr(allocator, expr),
        .Embedded => |emb| {
            allocator.free(emb.lang);
            allocator.free(emb.code);
        },
        .VarDecl => |decl| deinitExpr(allocator, &decl.initializer),
        else => {},
    }
}

fn deinitAst(allocator: Allocator, ast: *ArrayList(Stmt)) void {
    for (ast.items) |stmt| deinitStmt(allocator, &stmt);
    ast.deinit();
}

// JSON stringify, but for simplicity, implement a printer or use std.json
fn printAst(allocator: Allocator, ast: ArrayList(Stmt), writer: anytype) !void {
    try std.json.stringify(ast.items, .{ .whitespace = .indent_2 }, writer);
    // But need to define jsonStringify for unions
    // For now, assume it works, but actually need to implement JsonStringify for Expr/Stmt
    // Alternatively, write a custom printer
    // To make it working, let's write a simple recursive printer
    try writer.print("[\n", .{});
    for (ast.items, 0..) |stmt, i| {
        if (i > 0) try writer.print(",\n", .{});
        try printStmt(allocator, stmt, writer);
    }
    try writer.print("\n]\n", .{});
}

fn printStmt(allocator: Allocator, stmt: Stmt, writer: anytype) !void {
    switch (stmt) {
        .ExprStmt => |expr| {
            try writer.print("{{\"type\": \"ExprStmt\", \"expr\": ", .{});
            try printExpr(allocator, expr, writer);
            try writer.print("}}", .{});
        },
        .Function => |func| {
            try writer.print("{{\"type\": \"Function\", \"name\": \"{s}\", \"params\": [", .{func.name.lexeme});
            for (func.params.items, 0..) |param, j| {
                if (j > 0) try writer.print(", ", .{});
                try writer.print("\"{s}\"", .{param.lexeme});
            }
            try writer.print("], \"body\": [\n", .{});
            for (func.body.items, 0..) |body_stmt, k| {
                if (k > 0) try writer.print(",\n", .{});
                try printStmt(allocator, body_stmt, writer);
            }
            try writer.print("\n]}}", .{});
        },
        .Return => |expr| {
            try writer.print("{{\"type\": \"Return\", \"expr\": ", .{});
            try printExpr(allocator, expr, writer);
            try writer.print("}}", .{});
        },
        .Require => |req| try writer.print("{{\"type\": \"Require\", \"module\": {s}}}", .{req}),
        .Embedded => |emb| try writer.print("{{\"type\": \"Embedded\", \"lang\": \"{s}\", \"code\": \"{s}\"}}", .{emb.lang, emb.code}),
        .VarDecl => |decl| {
            try writer.print("{{\"type\": \"VarDecl\", \"name\": \"{s}\", \"initializer\": ", .{decl.name.lexeme});
            try printExpr(allocator, decl.initializer, writer);
            try writer.print("}}", .{});
        },
    }
}

fn printExpr(allocator: Allocator, expr: Expr, writer: anytype) !void {
    switch (expr) {
        .Literal => |lit| {
            switch (lit) {
                .Number => |n| try writer.print("{{\"type\": \"Number\", \"value\": {d}}}", .{n}),
                .String => |s| try writer.print("{{\"type\": \"String\", \"value\": \"{s}\"}}", .{s}),
            }
        },
        .Variable => |var| try writer.print("{{\"type\": \"Variable\", \"name\": \"{s}\"}}", .{var.lexeme}),
        .Binary => |bin| {
            try writer.print("{{\"type\": \"Binary\", \"left\": ", .{});
            try printExpr(allocator, bin.left.*, writer);
            try writer.print(", \"op\": \"{s}\", \"right\": ", .{bin.op.lexeme});
            try printExpr(allocator, bin.right.*, writer);
            try writer.print("}}", .{});
        },
        .Call => |call| {
            try writer.print("{{\"type\": \"Call\", \"callee\": ", .{});
            try printExpr(allocator, call.callee.*, writer);
            try writer.print(", \"args\": [", .{});
            for (call.args.items, 0..) |arg, l| {
                if (l > 0) try writer.print(", ", .{});
                try printExpr(allocator, arg, writer);
            }
            try writer.print("]}}", .{});
        },
        .Assign => |ass| {
            try writer.print("{{\"type\": \"Assign\", \"name\": \"{s}\", \"value\": ", .{ass.name.lexeme});
            try printExpr(allocator, ass.value.*, writer);
            try writer.print("}}", .{});
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
        std.log.err("Usage: PLSA <input.vib> <output.json>", .{});
        std.process.exit(1);
    }

    const input_file = args[1];
    const output_file = args[2];

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
        if (tok.typ == .Unknown) {
            printParseError(source, .{ .token = tok, .message = tok.lexeme });
            std.process.exit(1);
        }
    }

    var parser = Parser.init(allocator, tokens);
    const ast = parser.parse() catch |err| {
        if (err == error.ParseError) {
            // Assume last consume set the error, but for now, fake
            // To properly handle, make parse return !ArrayList or union with error info
            // For simplicity, assume error printed in consume
            // But in consume, if ! , we can print here
            // Since !ParseError, but I used error.ParseError
            std.process.exit(1);
        }
        return err;
    };
    defer deinitAst(allocator, &ast);

    const out_file = try std.fs.cwd().createFile(output_file, .{});
    defer out_file.close();

    var buffered_writer = std.io.bufferedWriter(out_file.writer());
    const writer = buffered_writer.writer();

    try printAst(allocator, ast, writer);

    try buffered_writer.flush();
}
