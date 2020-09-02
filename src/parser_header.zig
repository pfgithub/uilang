const tknzr = @import("tokenizer.zig");
const Token = tknzr.Token;
const Tokenizer = tknzr.Tokenizer;
const std = @import("std");
const Alloc = std.mem.Allocator;

pub const Parser = struct {
    arena: *Alloc,
    tokenizer: Tokenizer,
    tokens: std.ArrayList(Token),
    tkpos: usize = 0,
    errors: ?[]const u8 = null,
    pub fn init(alloc: *Alloc, code: []const u8) Parser {
        return .{
            .arena = alloc,
            .tokenizer = Tokenizer.init(code),
            .tokens = std.ArrayList(Token).init(alloc),
        };
    }
    pub fn deinit(parser: *Parser) void {
        parser.tokens.deinit(); // don't keep pointers to things from this list
    }
    pub fn err(parser: *Parser, message: []const u8) ParseError {
        // cancel bit
        parser.errors = message;
        return ParseError.ParseError;
    }
    pub fn nextToken(parser: *Parser) ParseError!?Token {
        if (parser.tkpos >= parser.tokens.items.len) {
            const nextToken_ = parser.tokenizer.next() catch return parser.err("bad token");
            try parser.tokens.append(nextToken_ orelse return null);
        }
        defer parser.tkpos += 1;
        return parser.tokens.items[parser.tkpos];
    }
    pub fn startBit(parser: Parser) usize {
        return parser.tkpos;
    }
    pub fn cancelBit(parser: *Parser, prevPos: usize) void {
        parser.tkpos = prevPos;
    }
};

pub const ParseError = error{
    OutOfMemory,
    ParseError,
};

fn _parseToken(parser: *Parser, tokenKind: Token.Type, expectedText: ?[]const u8) ParseError!Token {}
