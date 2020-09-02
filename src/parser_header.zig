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
