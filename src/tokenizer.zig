// why not just a union of name: []const u8?
pub const Token = struct {
    pub const Type = enum {
        identifier,
        string_start,
        string_chars,
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
};

pub const Tokenizer = struct {
    statev: [5]State,
    sv: usize,
    text: []const u8,
    current: usize,
    pub fn init(text: []const u8) Tokenizer {
        return .{
            .statev = [_]State{ .main, undefined, undefined, undefined, undefined },
            .sv = 0,
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
    fn state(tkr: Tokenizer) State {
        return tkr.statev[tkr.sv];
    }
    fn pushState(tkr: *Tokenizer, ns: State) void {
        tkr.sv += 1;
        tkr.statev[tkr.sv] = ns;
    }
    fn popState(tkr: *Tokenizer) void {
        if (tkr.sv == 0) tkr.sv += 1; // TODO (end)
        tkr.sv -= 1;
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
            switch (tkr.state()) {
                .main => {
                    switch (tkr.peek()) {
                        0 => return null,
                        'a'...'z', 'A'...'Z' => tkr.pushState(.identifier),
                        ' ', '\n', '\t' => _ = {
                            _ = tkr.take();
                            start = tkr.current;
                        },
                        else => |char| {
                            inline for ("[]{}();:,=|") |c| {
                                if (char == c) {
                                    _ = tkr.take();
                                    return tkr.token(start, .punctuation);
                                }
                            }
                            return error.IllegalCharacter;
                        },
                    }
                },
                .identifier => switch (tkr.peek()) {
                    'a'...'z', 'A'...'Z', '0'...'9' => _ = tkr.take(),
                    else => {
                        tkr.popState();
                        return tkr.token(start, .identifier);
                    },
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
        \\widget counter() {
    ;
    const expected = [_]Token.Type{ .identifier, .identifier, .punctuation, .punctuation, .punctuation };
    const expcdtxt = [_][]const u8{ "widget", "counter", "(", ")", "{" };
    testTokenizer(code, &expected, &expcdtxt);
}
