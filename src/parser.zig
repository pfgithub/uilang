const tknzr = @import("tokenizer.zig");
const Token = tknzr.Token;
const Tokenizer = tknzr.Tokenizer;
const std = @import("std");
const Alloc = std.mem.Allocator;
const ast = @import("ast.zig");

pub const Parser = struct {
    alloc: *Alloc,
    tokenizer: Tokenizer,
    errors: ?[]const u8 = null,
    pub fn init(alloc: *Alloc, code: []const u8) Parser {
        return .{
            .alloc = alloc,
            .tokenizer = Tokenizer.init(code),
        };
    }
    pub fn err(parser: *Parser, message: []const u8) ParseError {
        parser.errors = message;
        return ParseError.ParseError;
    }
};

const ParseError = error{
    OutOfMemory,
    ParseError,
};

// this is bad, there is no way for it to cancel if it's not an expression unless idk
pub fn parseExpression(parser: *Parser) ParseError!ast.Expression {
    const tok0 = parser.tokenizer.next() catch return parser.err("bad token");
    const token = tok0 orelse return parser.err("over");
    switch (token.kind) {
        .identifier => {
            // check if next token is an identifier
        },
        .punctuation => {
            if (token.text.len != 1) unreachable;
            switch (token.text[0]) {
                '.' => {},
                ':' => {},
                '|' => {},
                '{' => {},
            }
        },
        else => |kind| return parse.err("unexpected kind {}"),
    }
}
