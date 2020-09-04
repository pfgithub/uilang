const State = enum {
    main,
    identifier,
    string,
    string_dblquote,
    string_ending,
    string_ending_dblquote,
    comment,
    number,
};
// why not just a union of name: []const u8?
pub const Token = struct {
    pub const TokenType = enum {
        identifier,
        string_start,
        string,
        string_escape,
        string_end,
        punctuation,
        number,
    };
    kind: TokenType,
    text: []const u8,
    start: usize,
};
pub const Tokenizer = struct {
    state: State,
    text: []const u8,
    current: usize,
    pub fn init(text: []const u8) Tokenizer {
        return .{
            .state = .main,
            .text = text,
            .current = 0,
        };
    }

    fn peek(tkr: Tokenizer) u8 {
        if (tkr.current >= tkr.text.len) return 0;
        return tkr.text[tkr.current];
    }
    fn take(tkr: *Tokenizer) u8 {
        defer tkr.current += 1;
        return tkr.peek();
    }
    fn token(tkr: *Tokenizer, start: usize, ttype: Token.TokenType) Token {
        return .{
            .kind = ttype,
            .text = tkr.text[start..tkr.current],
            .start = start,
        };
    }

    pub fn next(tkr: *Tokenizer) !?Token {
        var start = tkr.current;
        while (true) {
            switch (tkr.state) {
                .main => {
                    switch (tkr.peek()) {
                        0 => return null,
                        'a'...'z', 'A'...'Z', '_', 128...255 => tkr.state = .identifier,
                        '0'...'9' => tkr.state = .number,
                        ' ', '\n', '\t' => _ = {
                            _ = tkr.take();
                            start = tkr.current;
                        },
                        '\'' => {
                            _ = tkr.take();
                            tkr.state = .string;
                            return tkr.token(start, .string_start);
                        },
                        '"' => {
                            _ = tkr.take();
                            tkr.state = .string_dblquote;
                            return tkr.token(start, .string_start);
                        },
                        '/' => {
                            _ = tkr.take();
                            if (tkr.peek() != '/') {
                                return tkr.token(start, .punctuation);
                            }
                            _ = tkr.take();
                            tkr.state = .comment;
                        },
                        else => |char| {
                            inline for ("[]{}();:,=|?<>!#*+/-.") |c| {
                                if (char == c) {
                                    _ = tkr.take();
                                    return tkr.token(start, .punctuation);
                                }
                            }
                            std.debug.panic("Illegal character: `{c}`", .{char});
                            // return error.IllegalCharacter;
                        },
                    }
                },
                .identifier => switch (tkr.peek()) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_', 128...255 => _ = tkr.take(),
                    else => {
                        tkr.state = .main;
                        return tkr.token(start, .identifier);
                    },
                },
                .number => switch (tkr.peek()) {
                    '0'...'9' => _ = tkr.take(),
                    else => {
                        tkr.state = .main;
                        return tkr.token(start, .number);
                    },
                },
                .string => switch (tkr.peek()) {
                    0, '\n' => return error.IllegalCharacter,
                    '\'' => {
                        tkr.state = .string_ending;
                        return tkr.token(start, .string);
                    },
                    else => _ = tkr.take(),
                },
                .string_dblquote => switch (tkr.peek()) {
                    0, '\n' => return error.IllegalCharacter,
                    '"' => {
                        tkr.state = .string_ending_dblquote;
                        return tkr.token(start, .string);
                    },
                    else => _ = tkr.take(),
                },
                .string_ending => switch (tkr.peek()) {
                    '\'' => {
                        tkr.state = .main;
                        _ = tkr.take();
                        return tkr.token(start, .string_end);
                    },
                    else => unreachable, // shouldn't be in this state
                },
                .string_ending_dblquote => switch (tkr.peek()) {
                    '"' => {
                        tkr.state = .main;
                        _ = tkr.take();
                        return tkr.token(start, .string_end);
                    },
                    else => unreachable, // shouldn't be in this state
                },
                .comment => switch (tkr.peek()) {
                    0, '\n' => tkr.state = .main,
                    else => _ = tkr.take(),
                },
            }
        }
    }
};

const std = @import("std");
const Alloc = std.mem.Allocator;

const ___ = @This();
fn __aToString(comptime a: anytype) []const u8 {
    return @tagName(a); // todo support strings too
}
fn GetResType(comptime aname: []const u8) type {
    if (!@hasDecl(___, aname)) @compileError("unknown type " ++ aname);
    return @field(___, aname);
}
pub fn parse(alloc: *Alloc, code: []const u8, comptime a: anytype) !GetResType(__aToString(a)) {
    const aname = comptime __aToString(a);
    const ResType = GetResType(aname);
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
    cpos: usize = 0,
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

        const restoken = parser.tokens.items[parser.tkpos];
        parser.cpos = restoken.start + restoken.text.len;
        return restoken;
    }
    fn startBit(parser: Parser) usize {
        return parser.tkpos;
    }
    fn cancelBit(parser: *Parser, prevPos: usize) void {
        parser.tkpos = prevPos;

        if (parser.tkpos == 0) parser.cpos = 0 else {
            const restoken = parser.tokens.items[parser.tkpos - 1];
            parser.cpos = restoken.start + restoken.text.len;
        }
    }
};

pub const ParseError = error{
    OutOfMemory,
    ParseError,
};

fn _parseToken(parser: *Parser, tokenKind: Token.TokenType, expectedText: ?[]const u8) ParseError!Token {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const tok = (try parser.nextToken()) orelse return parser.err("err");
    if (tok.kind != tokenKind) return parser.err("err");
    if (expectedText) |txt| if (!std.mem.eql(u8, tok.text, txt)) return parser.err("err");

    return tok;
}
