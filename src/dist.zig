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

pub const Main = _3;
const _3 = struct {
    hello: _0,
    world: _2,
};
const _0 = *Hello;
const _2 = ?_1;
const _1 = *World;
fn parse_4(parser: *Parser) ParseError!_3 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const _6 = try parse_5(parser);
    const _8 = try parse_7(parser);
    return _3{
        .hello = _6,
        .world = _8,
    };
}
fn parse_5(parser: *Parser) ParseError!_0 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const _9 = try parseHello(parser);
    const _9 = try parser.alloc.create(@TypeOf(_10));
    _10.* = _9;
    return _10;
}
fn parse_7(parser: *Parser) ParseError!_2 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return _11(parser) catch |e| switch (e) {
        error.OutOfMemory => return e,
        error.ParseError => return null, // note that the called function already cancelBit'd so it's ok
    };
}
fn parse_11(parser: *Parser) ParseError!_1 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const _12 = try parseWorld(parser);
    const _12 = try parser.alloc.create(@TypeOf(_13));
    _13.* = _12;
    return _13;
}
pub const parseMain = _4;
pub const Hello = _14;
const _14 = []const u8;
fn parse_15(parser: *Parser) ParseError!_14 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return try _parseToken(parser, .identifier, "hello");
}
pub const parseHello = _15;
pub const World = _16;
const _16 = []const u8;
fn parse_17(parser: *Parser) ParseError!_16 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return try _parseToken(parser, .identifier, "world");
}
pub const parseWorld = _17;
