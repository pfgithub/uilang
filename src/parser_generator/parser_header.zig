const State = enum {
    main,
    identifier,
    string,
    string_dblquote,
    string_ending,
    string_ending_dblquote,
    comment,
    number,
    multiline_string,
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
        multiline_string,
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
                        '\\' => {
                            _ = tkr.take();
                            if (tkr.peek() != '\\') {
                                return tkr.token(start, .punctuation);
                            }
                            _ = tkr.take();
                            tkr.state = .multiline_string;
                            start = tkr.current;
                        },
                        '/' => {
                            _ = tkr.take();
                            if (tkr.peek() != '/') {
                                return tkr.token(start, .punctuation);
                            }
                            _ = tkr.take();
                            tkr.state = .comment;
                            start = tkr.current;
                        },
                        else => |char| {
                            inline for ("[]{}();:,=|?<>!#*+/-.@^&") |c| {
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
                        // TODO support keywords
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
                    '\\' => {
                        @panic("TODO backslash");
                    },
                    else => _ = tkr.take(),
                },
                .string_dblquote => switch (tkr.peek()) {
                    0, '\n' => return error.IllegalCharacter,
                    '"' => {
                        tkr.state = .string_ending_dblquote;
                        return tkr.token(start, .string);
                    },
                    '\\' => {
                        @panic("TODO backslash");
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
                    0, '\n' => {
                        tkr.state = .main;
                        start = tkr.current;
                    },
                    else => _ = tkr.take(),
                },
                .multiline_string => switch (tkr.peek()) {
                    0, '\n' => {
                        tkr.state = .main;
                        return tkr.token(start, .multiline_string);
                    },
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

pub fn printSyntaxHighlight(text: []const u8, out: anytype) @TypeOf(out).Error!void {
    var tokenizer = Tokenizer.init(text);
    var prev_token: Token = Token{ .kind = .identifier, .text = ".", .start = 0 };
    while (true) {
        const start = tokenizer.current;
        const nextToken = tokenizer.next() catch {
            try out.print("\x1b[31m{}\x1b(B\x1b[m", .{tokenizer.text[start..]});
            break;
        };
        const token = nextToken orelse {
            try out.writeAll(tokenizer.text[start..]);
            break;
        };
        const here = tokenizer.current;
        const strstart = here - token.text.len;
        try out.writeAll("\x1b(B\x1b[m");
        for (tokenizer.text[start..strstart]) |char| {
            switch (char) {
                '\t' => try out.writeAll("→   "),
                else => try out.writeByte(char),
            }
        }
        switch (token.kind) {
            .identifier => blk: {
                if (prev_token.kind == .punctuation) {
                    switch (prev_token.text[0]) {
                        '@' => break :blk try out.writeAll("\x1b[96m"),
                        '\\' => break :blk try out.writeAll("\x1b[93m"),
                        '.' => break :blk try out.writeAll("\x1b[38;5;224m"),
                        else => {},
                    }
                }
                if (std.meta.stringToEnum(enum { @"pub", @"state", @"const", @"memo", @"var", @"let", @"trigger" }, token.text)) |_| {
                    try out.writeAll("\x1b[94m");
                } else if (std.meta.stringToEnum(enum { @"widget", @"fn", @"if", @"else", @"return", @"while", @"switch", @"for", @"or", @"and", @"try", @"catch", @"orelse", @"defer", @"once" }, token.text)) |_| {
                    try out.writeAll("\x1b[93m");
                } else if (std.meta.stringToEnum(enum { @"html", @"string", @"attribute", @"f64", @"i51", @"void" }, token.text)) |_| {
                    try out.writeAll("\x1b[94m");
                } else if (std.meta.stringToEnum(enum { @"null", @"false", @"true" }, token.text)) |_| {
                    try out.writeAll("\x1b[97m");
                }
            },
            .string_start => try out.writeAll("\x1b[32m"),
            .string_end => try out.writeAll("\x1b[32m"),
            .string => try out.writeAll("\x1b[92m"),
            .string_escape => try out.writeAll("\x1b[94m"),
            .punctuation => switch (token.text[0]) {
                '@' => try out.writeAll("\x1b[36m"),
                ':', '=', '#', '^', '|', '.', '!', '>', '<', '+', '-', '*', '/', '\\' => try out.writeAll("\x1b[93m"),
                else => try out.writeAll("\x1b[38;5;240m"),
            },
            .number => try out.writeAll("\x1b[96m"),
            .multiline_string => try out.writeAll("\x1b[92m"),
        }
        for (tokenizer.text[strstart..here]) |char| {
            switch (char) {
                '\t' => try out.writeAll("    "),
                else => try out.writeByte(char),
            }
        }
        try out.writeAll("\x1b(B\x1b[m");
        prev_token = token;
    }
}

pub fn printErrorPos(text: []const u8, message: []const u8, epos: usize, out: anytype) @TypeOf(out).Error!void {
    // todo rewrite this to be sane
    var lyn: usize = 0;
    var col: usize = 0;
    var latestLine: usize = 0;
    for (text) |char, i| {
        if (epos == i) break;
        col += 1;
        if (char == '\n') {
            lyn += 1;
            col = 0;
            latestLine = i + 1;
        }
    }
    var lineText = std.mem.span(@ptrCast([*:'\n']const u8, &text[latestLine]));
    try out.print("\x1b[1m\x1b[97m./file:{}:{}: \x1b[31merror: \x1b[97m{s}\x1b(B\x1b[m\n", .{ lyn + 1, col + 1, message });
    // would it be bad to tokenize and syntax highlight lineText?
    try printSyntaxHighlight(lineText, out);
    try out.print("\x1b(B\x1b[m\n", .{});
    var i: usize = 0;
    while (i < col) : (i += 1) {
        switch (lineText[i]) {
            '\t' => try out.print("    ", .{}),
            else => try out.print(" ", .{}),
        }
    }
    try out.print("^\n", .{});
}
pub fn parse(alloc: *Alloc, code: []const u8, comptime a: anytype) !GetResType(__aToString(a)) {
    const aname = comptime __aToString(a);
    const ResType = GetResType(aname);
    const resfn = @field(___, "parse" ++ aname);

    var parser = Parser.init(alloc, code);
    defer parser.deinit();

    const out = std.io.getStdErr().writer();

    // TODO: @resultLocation().* = …
    const outmain = resfn(&parser) catch |e| switch (e) {
        error.OutOfMemory => return e,
        error.Recoverable, error.Unrecoverable => {
            if (parser.unrecoverableStats) |stats| {
                try printErrorPos(parser.tokenizer.text, stats.message, stats.pos, out);
                try printErrorPos(parser.tokenizer.text, "(farthest)", parser.farthest, out);
            } else {
                try printErrorPos(parser.tokenizer.text, parser.errors.?, parser.farthest, out);
            }
            std.debug.panic("Parsing failed\n", .{});
        },
    };
    if ((try parser.nextToken())) |tok| {
        try printErrorPos(parser.tokenizer.text, "Remaining token", tok.start, out);
        try printErrorPos(parser.tokenizer.text, "(farthest)", parser.farthest, out);
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
    farthest: usize = 0,
    errors: ?[]const u8 = null,
    unrecoverableStats: ?struct {
        pos: usize,
        farthest: usize,
        message: []const u8,
    } = null,
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
        return ParseError.Recoverable;
    }
    fn unrecoverableError(parser: *Parser, message: []const u8) ParseError {
        parser.errors = message;
        parser.unrecoverableStats = .{
            .pos = parser.cpos,
            .message = message,
            .farthest = parser.farthest,
        };
        return ParseError.Unrecoverable;
    }
    fn nextToken(parser: *Parser) ParseError!?Token {
        if (parser.tkpos >= parser.tokens.items.len) {
            const nextToken_ = parser.tokenizer.next() catch return parser.unrecoverableError("bad token");
            try parser.tokens.append(nextToken_ orelse return null);
        }
        defer parser.tkpos += 1;

        const restoken = parser.tokens.items[parser.tkpos];
        parser.cpos = restoken.start + restoken.text.len;
        parser.farthest = parser.cpos;
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
    Unrecoverable,
    OutOfMemory,
    Recoverable,
};

fn _parseToken(parser: *Parser, tokenKind: Token.TokenType, expectedText: ?[]const u8) ParseError!Token {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const tok = (try parser.nextToken()) orelse return parser.err("err");
    if (tok.kind != tokenKind) return parser.err("err");
    if (expectedText) |txt| if (!std.mem.eql(u8, tok.text, txt)) return parser.err("err");

    return tok;
}

test "" {
    std.meta.refAllDecls(@This());
}
