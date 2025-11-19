const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

// Reuse TokenType, Token, Lexer from translator
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

const Token = struct {
    typ: TokenType,
    lexeme: []const u8,
    line: usize,
};

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
        return .Identifier;
    }

    fn makeToken(self: *Lexer, typ: TokenType) Token {
        return .{ .typ = typ, .lexeme = self.source[self.start..self.current], .line = self.line };
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        return .{ .typ = .Unknown, .lexeme = message, .line = self.line };
    }
};

// AST from translator
const Value = union(enum) {
    Number: f64,
    String: []const u8,
    Function: *Function,
};

const Function = struct {
    name: []const u8,
    params: []Token,
    body: []Stmt,
};

const Expr = union(enum) {
    Literal: struct { value: Value },
    Binary: struct { left: *Expr, op: Token, right: *Expr },
    Variable: Token,
    Call: struct { callee: *Expr, args: []Expr },
};

const Stmt = union(enum) {
    ExprStmt: Expr,
    Function: Function,
    Return: Expr,
    Require: []const u8,
    Embedded: struct { lang: []const u8, code: []const u8 },
    VarDecl: struct { name: Token, initializer: Expr },
};

// Updated Parser to handle variables etc.
const Parser = struct {
    tokens: []Token,
    current: usize = 0,
    allocator: Allocator,

    fn init(allocator: Allocator, tokens: []Token) Parser {
        return .{ .tokens = tokens, .allocator = allocator };
    }

    fn parse(self: *Parser) !ArrayList(Stmt) {
        var statements = ArrayList(Stmt).init(self.allocator);
        defer statements.deinit(); // No, return it
        while (!self.isAtEnd()) {
            try statements.append(try self.declaration());
        }
        return statements;
    }

    fn declaration(self: *Parser) !Stmt {
        if (self.match(.Identifier) and self.match(.Equals)) {
            const name = self.previous(1);
            const init = try self.expression();
            return .{ .VarDecl = .{ .name = name, .initializer = init } };
        }
        return try self.statement();
    }

    fn statement(self: *Parser) !Stmt {
        if (self.match(.KeywordFunction)) return .{ .Function = try self.function() };
        if (self.match(.KeywordReturn)) return .{ .Return = try self.expression() };
        if (self.match(.KeywordRequire)) return .{ .Require = (try self.consume(.String, "Expect string after require.")).lexeme };
        if (self.match(.Hash)) return try self.embedded();
        return .{ .ExprStmt = try self.expression() };
    }

    fn function(self: *Parser) !Function {
        const name = try self.consume(.Identifier, "Expect function name.");
        _ = try self.consume(.LParen, "Expect '(' after function name.");
        var params = try ArrayList(Token).initCapacity(self.allocator, 0);
        if (!self.check(.RParen)) {
            while (true) {
                try params.append(try self.consume(.Identifier, "Expect parameter name."));
                if (!self.match(.Comma)) break;
            }
        }
        _ = try self.consume(.RParen, "Expect ')' after parameters.");
        _ = try self.consume(.LBracket, "Expect '[' before function body.");
        var body = try ArrayList(Stmt).initCapacity(self.allocator, 0);
        while (!self.check(.RBracket) and !self.isAtEnd()) {
            try body.append(try self.declaration());
        }
        _ = try self.consume(.RBracket, "Expect ']' after function body.");
        return .{ .name = name.lexeme, .params = params.items, .body = body.items };
    }

    fn embedded(self: *Parser) !Stmt {
        var lang_buf = ArrayList(u8).init(self.allocator);
        defer lang_buf.deinit();
        while (!self.match(.Equals)) {
            try lang_buf.appendSlice(self.advance().lexeme);
        }
        _ = try self.consume(.LBracket, "Expect '[' after language name.");
        var code_buf = ArrayList(u8).init(self.allocator);
        defer code_buf.deinit();
        var depth: i32 = 1;
        while (depth > 0 and !self.isAtEnd()) {
            const tok = self.advance();
            try code_buf.appendSlice(tok.lexeme);
            if (tok.typ == .LBracket) depth += 1;
            if (tok.typ == .RBracket) depth -= 1;
        }
        if (depth > 0) return error.ParseError;
        return .{ .Embedded = .{ .lang = try lang_buf.toOwnedSlice(), .code = try code_buf.toOwnedSlice() } };
    }

    fn expression(self: *Parser) !Expr {
        return try self.assignment();
    }

    fn assignment(self: *Parser) !Expr {
        const expr = try self.additive();
        if (self.match(.Equals)) {
            const value = try self.assignment();
            if (expr == .Variable) {
                return .{ .Assign = .{ .name = expr.Variable, .value = value } }; // Add Assign to Expr
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
            expr = .{ .Binary = .{ .left = try self.allocator.create(Expr{expr}), .op = op, .right = try self.allocator.create(Expr{right}) } };
        }
        return expr;
    }

    fn multiplicative(self: *Parser) !Expr {
        var expr = try self.primary();
        while (self.match(.Star) or self.match(.Slash)) {
            const op = self.previous();
            const right = try self.primary();
            expr = .{ .Binary = .{ .left = try self.allocator.create(Expr{expr}), .op = op, .right = try self.allocator.create(Expr{right}) } };
        }
        return expr;
    }

    fn primary(self: *Parser) !Expr {
        if (self.match(.Number)) return .{ .Literal = .{ .value = .{ .Number = try std.fmt.parseFloat(f64, self.previous().lexeme) } } };
        if (self.match(.String)) return .{ .Literal = .{ .value = .{ .String = self.previous().lexeme } } };
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
                return .{ .Call = .{ .callee = try self.allocator.create(Expr{ .Variable = self.tokens[self.current - 2] }), .args = args.items } };
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

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().typ == .Eof;
    }

    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }

    fn previousN(self: *Parser, n: usize) Token {
        return self.tokens[self.current - n];
    }

    fn consume(self: *Parser, typ: TokenType, message: []const u8) !Token {
        if (self.check(typ)) return self.advance();
        std.log.err("{s} at line {}", .{message, self.peek().line});
        return error.ParseError;
    }
};

// Interpreter
const Frame = struct {
    locals: std.StringHashMap(Value),
    return_value: ?Value = null,
};

const Interpreter = struct {
    allocator: Allocator,
    globals: std.StringHashMap(Value),
    stack: ArrayList(Value),
    frames: ArrayList(Frame),

    fn init(allocator: Allocator) Interpreter {
        return .{
            .allocator = allocator,
            .globals = std.StringHashMap(Value).init(allocator),
            .stack = ArrayList(Value).init(allocator),
            .frames = ArrayList(Frame).init(allocator),
        };
    }

    fn deinit(self: *Interpreter) void {
        self.globals.deinit();
        self.stack.deinit();
        self.frames.deinit();
    }

    fn interpret(self: *Interpreter, stmts: []Stmt) !void {
        try self.frames.append(Frame{ .locals = std.StringHashMap(Value).init(self.allocator) });
        for (stmts) |stmt| {
            try self.execute(stmt);
        }
    }

    fn execute(self: *Interpreter, stmt: Stmt) !void {
        switch (stmt) {
            .ExprStmt => |expr| {
                const val = try self.evaluate(expr);
                _ = val; // Pop or something, but for now ignore
            },
            .Function => |func| {
                try self.currentFrame().locals.put(func.name, .{ .Function = try self.allocator.create(Function{func}) });
            },
            .Return => |expr| {
                self.currentFrame().return_value = try self.evaluate(expr);
            },
            .VarDecl => |decl| {
                const val = try self.evaluate(decl.initializer);
                try self.currentFrame().locals.put(decl.name.lexeme, val);
            },
            .Require => |mod| {
                std.log.warn("Require not implemented: {s}", .{mod});
            },
            .Embedded => |emb| {
                std.log.warn("Embedded not implemented: lang {s}, code {s}", .{emb.lang, emb.code});
            },
        }
    }

    fn evaluate(self: *Interpreter, expr: Expr) !Value {
        switch (expr) {
            .Literal => |lit| return lit.value,
            .Variable => |var| {
                if (self.currentFrame().locals.get(var.lexeme)) |val| return val;
                if (self.globals.get(var.lexeme)) |val| return val;
                return error.UndefinedVariable;
            },
            .Binary => |bin| {
                const left = try self.evaluate(bin.left.*);
                const right = try self.evaluate(bin.right.*);
                switch (bin.op.typ) {
                    .Plus => {
                        if (left == .Number and right == .Number) return .{ .Number = left.Number + right.Number };
                        return error.TypeMismatch;
                    },
                    .Minus => {
                        if (left == .Number and right == .Number) return .{ .Number = left.Number - right.Number };
                    },
                    .Star => {
                        if (left == .Number and right == .Number) return .{ .Number = left.Number * right.Number };
                    },
                    .Slash => {
                        if (left == .Number and right == .Number) return .{ .Number = left.Number / right.Number };
                    },
                    else => return error.InvalidOperator,
                }
            },
            .Call => |call| {
                const callee = try self.evaluate(call.callee.*);
                if (callee != .Function) return error.NotCallable;
                const func = callee.Function;
                if (func.params.len != call.args.len) return error.ArityMismatch;
                try self.frames.append(Frame{ .locals = std.StringHashMap(Value).init(self.allocator) });
                for (func.params, call.args) |param, arg| {
                    try self.currentFrame().locals.put(param.lexeme, try self.evaluate(arg));
                }
                for (func.body) |body_stmt| {
                    try self.execute(body_stmt);
                    if (self.currentFrame().return_value) |_| break;
                }
                const ret = self.currentFrame().return_value orelse .{ .Number = 0.0 }; // Default return 0
                _ = self.frames.pop();
                return ret;
            },
            else => return error.NotImplemented,
        }
    }

    fn currentFrame(self: *Interpreter) *Frame {
        return &self.frames.items[self.frames.items.len - 1];
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.log.err("Usage: vm <input.vib>", .{});
        std.process.exit(1);
    }

    const input_file = args[1];

    const file = try std.fs.cwd().openFile(input_file, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(source);

    var lexer = Lexer.init(source);
    var tokens_list = ArrayList(Token).init(allocator);
    defer tokens_list.deinit();

    while (true) {
        const tok = lexer.nextToken();
        try tokens_list.append(tok);
        if (tok.typ == .Eof) break;
    }

    var parser = Parser.init(allocator, tokens_list.items);
    const ast = try parser.parse();

    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    try interpreter.interpret(ast.items);

    // For now, this is an interpreter for .vib, but in future, load .vib-vm which could be serialized AST or bytecode.
    // To make it VM, we can serialize AST to file, but for "działający" this runs the code.
}
