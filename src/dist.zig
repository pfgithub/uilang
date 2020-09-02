//! Autogenerated Code.
//! Manual edits may be overwritten on rebuild.

const tknzr = @import("tokenizer.zig");
const Token = tknzr.Token;
const Tokenizer = tknzr.Tokenizer;
const std = @import("std");
const Alloc = std.mem.Allocator;

const ___ = @This();
fn __aToString(comptime a: anytype) []const u8 {
    return @tagName(a); // todo support strings too
}
pub fn parse(alloc: *Alloc, code: []const u8, comptime a: anytype) !@field(___, __aToString(a)) {
    const aname = comptime __aToString(a);
    const ResType = @field(___, aname);
    const resfn = @field(___, "parse" ++ aname);

    var parser = Parser.init(alloc, code);
    defer parser.deinit();

    // TODO: @resultLocation().* = …
    const outmain = try resfn(&parser);
    if ((try parser.nextToken())) |tok| {
        std.debug.panic("Remaining token: {}\n", .{tok});
    }
    return outmain;
}
pub const Parser = struct {
    alloc: *Alloc,
    tokenizer: Tokenizer,
    tokens: std.ArrayList(Token),
    tkpos: usize = 0,
    errors: ?[]const u8 = null,
    fn init(alloc: *Alloc, code: []const u8) Parser {
        return .{
            .alloc = alloc,
            .tokenizer = Tokenizer.init(code),
            .tokens = std.ArrayList(Token).init(alloc),
        };
    }
    fn deinit(parser: *Parser) void {
        parser.tokens.deinit();
    }
    fn err(parser: *Parser, message: []const u8) ParseError {
        parser.errors = message;
        return ParseError.ParseError;
    }
    fn nextToken(parser: *Parser) ParseError!?Token {
        if (parser.tkpos >= parser.tokens.items.len) {
            const nextToken_ = parser.tokenizer.next() catch return parser.err("bad token");
            try parser.tokens.append(nextToken_ orelse return null);
        }
        defer parser.tkpos += 1;
        return parser.tokens.items[parser.tkpos];
    }
    fn startBit(parser: Parser) usize {
        return parser.tkpos;
    }
    fn cancelBit(parser: *Parser, prevPos: usize) void {
        parser.tkpos = prevPos;
    }
};

pub const ParseError = error{
    OutOfMemory,
    ParseError,
};

fn _parseToken(parser: *Parser, tokenKind: Token.Type, expectedText: ?[]const u8) ParseError!Token {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const tok = (try parser.nextToken()) orelse return parser.err("err");
    if (tok.kind != tokenKind) return parser.err("err");
    if (expectedText) |txt| if (!std.mem.eql(u8, tok.text, txt)) return parser.err("err");

    return tok;
}

pub const Main = _3;
const _3 = struct {
    hello: _0,
    world: _2,
};
const _0 = *Hello;
const _2 = ?_1;
const _1 = *World;
fn _4(parser: *Parser) ParseError!_3 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const _6 = try _5(parser);
    const _8 = try _7(parser);
    return _3{
        .hello = _6,
        .world = _8,
    };
}
fn _5(parser: *Parser) ParseError!_0 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const _9 = try parseHello(parser);
    const _10 = try parser.alloc.create(@TypeOf(_9));
    _10.* = _9;
    return _10;
}
fn _7(parser: *Parser) ParseError!_2 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return _11(parser) catch |e| switch (e) {
        error.OutOfMemory => return e,
        error.ParseError => return null, // note that the called function already cancelBit'd so it's ok
    };
}
fn _11(parser: *Parser) ParseError!_1 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const _12 = try parseWorld(parser);
    const _13 = try parser.alloc.create(@TypeOf(_12));
    _13.* = _12;
    return _13;
}
pub const parseMain = _4;
pub const Hello = _14;
const _14 = []const u8;
fn _15(parser: *Parser) ParseError!_14 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return (try _parseToken(parser, .identifier, "hello")).text;
}
pub const parseHello = _15;
pub const World = _16;
const _16 = []const u8;
fn _17(parser: *Parser) ParseError!_16 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return (try _parseToken(parser, .identifier, "world")).text;
}
pub const parseWorld = _17;
