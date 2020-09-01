// why not just a union of name: []const u8?
pub const Token = struct {
    pub const Type = enum {
        identifier,
        string_start,
        string,
        string_escape,
        string_end,
        punctuation,
    };
    kind: Type,
    text: []const u8,
};

const State = enum {
    main,
    identifier,
    string,
    string_ending,
    comment,
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
    fn token(tkr: *Tokenizer, start: usize, ttype: Token.Type) Token {
        return .{
            .kind = ttype,
            .text = tkr.text[start..tkr.current],
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
                        ' ', '\n', '\t' => _ = {
                            _ = tkr.take();
                            start = tkr.current;
                        },
                        '\'' => {
                            _ = tkr.take();
                            tkr.state = .string;
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
                            inline for ("[]{}();:,=|?<>!") |c| {
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
                .string => switch (tkr.peek()) {
                    0, '\n' => return error.IllegalCharacter,
                    '\'' => {
                        tkr.state = .string_ending;
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
                .comment => switch (tkr.peek()) {
                    0, '\n' => tkr.state = .main,
                    else => _ = tkr.take(),
                },
            }
        }
    }
};

const std = @import("std");

fn testTokenizer(code: []const u8, expected: []const Token.Type, expcdtxt: []const []const u8) void {
    var tkr = Tokenizer.init(code);
    var i: usize = 0;
    if (expected.len != expcdtxt.len) @panic("bad lens");
    while (tkr.next() catch @panic("bad character")) |tok| : (i += 1) {
        if (i >= expected.len) std.debug.panic("got unexpected token {} {}", .{ tok, i });
        if (tok.kind != expected[i]) {
            std.debug.panic("expected {}, got {}", .{ expected[i], tok.kind });
        }
        if (!std.mem.eql(u8, tok.text, expcdtxt[i])) {
            std.debug.panic("expected \"{}\", got \"{}\"", .{ expcdtxt[i], tok.text });
        }
    }
}

test "tokenizer" {
    const code =
        \\file = decl[';']; // comment
    ;
    const expected = [_]Token.Type{ .identifier, .punctuation, .identifier, .punctuation, .string_start, .string, .string_end, .punctuation, .punctuation };
    const expcdtxt = [_][]const u8{ "file", "=", "decl", "[", "'", ";", "'", "]", ";" };
    testTokenizer(code, &expected, &expcdtxt);
}
