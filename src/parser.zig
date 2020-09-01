const tknzr = @import("tokenizer.zig");
const Token = tknzr.Token;
const Tokenizer = tknzr.Tokenizer;
const std = @import("std");
const Alloc = std.mem.Allocator;
const ast = @import("ast.zig");

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

const ParseError = error{
    OutOfMemory,
    ParseError,
};

// idk what to do about positions yet. todo.
const File = struct {
    decls: []Decl,
    pub fn print(file: File, out: anytype) @TypeOf(out).Error!void {
        for (file.decls) |decl| {
            try decl.print(out);
            try out.writeAll(";\n");
        }
    }
};
const Decl = struct {
    name: []const u8,
    value: *Component,
    pub fn print(decl: Decl, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll(decl.name);
        try out.writeAll(" = ");
        try decl.value.print(out);
    }
};
const Component = union(enum) {
    suffixop: struct { component: *Component, suffixop: *Suffixop },
    or_op: []Component, //, operator: OPERATOR eg
    p_op: []Component,
    decl_ref: *Identifier,
    parens: *Parens,
    string: *String,
    magic: *Magic,
    pub fn print(component: Component, out: anytype) @TypeOf(out).Error!void {
        switch (component) {
            .suffixop => |v| {
                try v.component.print(out);
                try v.suffixop.print(out);
            },
            .or_op => |v| for (v) |q, i| {
                if (i != 0) try out.writeAll(" | ");
                try q.print(out);
            },
            .p_op => |v| for (v) |q, i| {
                if (i != 0) try out.writeAll(" ");
                try q.print(out);
            },
            .decl_ref => |v| try v.print(out),
            .parens => |v| try v.print(out),
            .string => |v| try v.print(out),
            .magic => |v| try v.print(out),
        }
    }
};
const Parens = struct {
    component: *Component,
    pub fn print(parens: Parens, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll("(");
        try parens.component.print(out);
        try out.writeAll(")");
    }
};
const String = struct {
    const StringBit = union(enum) {
        string: []const u8,
        escape: []const u8,
    };
    bits: []StringBit,
    pub fn print(string: String, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll("\"");
        for (string.bits) |bit| {
            switch (bit) {
                .string => |v| try out.writeAll(v),
                .escape => |v| try out.writeAll(v),
            }
        }
        try out.writeAll("\"");
    }
};
const Magic = struct {
    name: *Identifier,
    args: []Component,
    pub fn print(magic: Magic, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll("#");
        try magic.name.print(out);
        try out.writeAll("(");
        for (magic.args) |arg, i| {
            if (i != 0) try out.writeAll(", ");
            try arg.print(out);
        }
        try out.writeAll(")");
    }
};
const Suffixop = union(enum) {
    nameset: *Nameset,
    array: *Array,
    optional: *Optional,
    pub fn print(suffix: Suffixop, out: anytype) @TypeOf(out).Error!void {
        switch (suffix) {
            .nameset => |ns| try ns.print(out),
            .array => |ar| try ar.print(out),
            .optional => |op| try op.print(out),
        }
    }
};

const Identifier = struct {
    name: []const u8,
    pub fn print(id: Identifier, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll(id.name);
    }
};

const Nameset = struct {
    name: ?*Identifier,
    pub fn print(ns: Nameset, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll("<");
        if (ns.name) |nm| try nm.print(out);
        try out.writeAll(">");
    }
};
const Array = struct {
    component: ?*Component,
    pub fn print(ar: Array, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll("[");
        if (ar.component) |cmpnt| try cmpnt.print(out);
        try out.writeAll("]");
    }
};
const Optional = struct {
    unused: u1 = 0,
    pub fn print(op: Optional, out: anytype) @TypeOf(out).Error!void {
        try out.writeAll("?");
    }
};

const STRING_START = Token;
const STRING = Token;
const STRING_END = Token;
const STRING_ESCAPE = Token;

// for now, error handling will be really useless.
// once we start generating this, error handling can have switch statements and stuff which make the code harder to read but better error handling.

/// file = decl[';']<decls>;
pub fn parseFile(parser: *Parser) ParseError!File {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    var resAL = std.ArrayList(Decl).init(parser.arena);

    while (true) {
        const parseResult = parseDecl(parser) catch break;
        try resAL.append(parseResult);
        _ = parseToken(parser, .punctuation, ";") catch break;
    }

    return File{
        .decls = resAL.toOwnedSlice(),
    };
}

/// decl = identifier<name> '=' component<value>;
pub fn parseDecl(parser: *Parser) ParseError!Decl {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    // name, value
    const name = parseToken(parser, .identifier, null) catch return parser.err("expected ident");
    _ = parseToken(parser, .punctuation, "=") catch return parser.err("expected =");
    const valueAlloc = try parser.arena.create(Component);
    valueAlloc.* = parseComponent(parser) catch return parser.err("expected component");
    return Decl{
        .name = name.text,
        .value = valueAlloc,
    };
}

/// component = _component_noop2["|", first, nolast, 1+]
/// eg a | b | c
/// | a | b | c
/// BUT NOT a | b | c |
pub fn parseComponent(parser: *Parser) ParseError!Component {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    var resAL = std.ArrayList(Component).init(parser.arena);
    _ = parseToken(parser, .punctuation, "|") catch {}; // optional first or
    while (true) {
        try resAL.append(parseComponent__NoOperator2(parser) catch return parser.err("last or disallowed"));
        _ = parseToken(parser, .punctuation, "|") catch break;
    }

    if (resAL.items.len == 0) return parser.err("no items");
    if (resAL.items.len == 1) return resAL.items[0];

    return Component{ .or_op = resAL.toOwnedSlice() };
}

/// _component_noop2 = _component_noop[1+]
fn parseComponent__NoOperator2(parser: *Parser) ParseError!Component {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    var resAL = std.ArrayList(Component).init(parser.arena);
    while (true) {
        try resAL.append(parseComponent__NoOperator(parser) catch break);
    }

    if (resAL.items.len == 0) return parser.err("no items");
    if (resAL.items.len == 1) return resAL.items[0];

    return Component{ .p_op = resAL.toOwnedSlice() };
}

/// _component_noop = _component_nosuffix suffixop[]
fn parseComponent__NoOperator(parser: *Parser) ParseError!Component {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    // 1: parseSingleComponent
    const resc = parseComponent__NoSuffix(parser) catch return parser.err("no");
    // 2: parse suffixop repeated
    var resSuffixes = std.ArrayList(Suffixop).init(parser.arena);

    while (true) {
        try resSuffixes.append(parseSuffixop(parser) catch break);
    }
    // 3: unwrap
    var topUnwrapped = resc;
    for (resSuffixes.items) |suffix| {
        const allocated = try parser.arena.create(Component);
        allocated.* = topUnwrapped;
        const allocatedSuffix = try parser.arena.create(Suffixop);
        allocatedSuffix.* = suffix;
        topUnwrapped = .{ .suffixop = .{ .component = allocated, .suffixop = allocatedSuffix } };
    }
    return topUnwrapped;
}

/// _component_nosuffix =
/// 	| identifier<decl_ref>
/// 	| parens
/// 	| string
/// 	| magic
fn parseComponent__NoSuffix(parser: *Parser) ParseError!Component {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    // parse the entire component
    // parse suffixop[]
    // unwrap those into components

    // | identifier<decl_ref>
    blk: {
        const v = parseToken(parser, .identifier, null) catch break :blk;
        const allocated = try parser.arena.create(Identifier);
        allocated.* = .{ .name = v.text };
        return Component{ .decl_ref = allocated };
    }

    // | parens
    blk: {
        const v = parseParens(parser) catch break :blk;
        const allocated = try parser.arena.create(@TypeOf(v));
        allocated.* = v;
        return Component{ .parens = allocated };
    }

    // | string
    blk: {
        const v = parseString(parser) catch break :blk;
        const allocated = try parser.arena.create(@TypeOf(v));
        allocated.* = v;
        return Component{ .string = allocated };
    }

    // | magic
    blk: {
        const v = parseMagic(parser) catch break :blk;
        const allocated = try parser.arena.create(@TypeOf(v));
        allocated.* = v;
        return Component{ .magic = allocated };
    }

    return parser.err("none of or");
}

/// suffixop =
///     | nameset
///     | array
///     | optional
/// ;
pub fn parseSuffixop(parser: *Parser) ParseError!Suffixop {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    // | nameset
    blk: {
        const v = parseNameset(parser) catch break :blk;
        const allocated = try parser.arena.create(@TypeOf(v));
        allocated.* = v;
        return Suffixop{ .nameset = allocated };
    }

    // | array
    blk: {
        const v = parseArray(parser) catch break :blk;
        const allocated = try parser.arena.create(@TypeOf(v));
        allocated.* = v;
        return Suffixop{ .array = allocated };
    }

    // | optional
    blk: {
        const v = parseOptional(parser) catch break :blk;
        const allocated = try parser.arena.create(@TypeOf(v));
        allocated.* = v;
        return Suffixop{ .optional = allocated };
    }

    return parser.err("none of or");
}

/// parens = '(' component ')';
pub fn parseParens(parser: *Parser) ParseError!Parens {
    _ = try parseToken(parser, .punctuation, "(");
    const component = try parseComponent(parser);
    _ = try parseToken(parser, .punctuation, ")");
    const allocatedComponent = try parser.arena.create(@TypeOf(component));
    allocatedComponent.* = component;
    return Parens{ .component = allocatedComponent };
}

/// string = STRING_START<> (STRING | STRING_ESCAPE<escape>)[]<bits> STRING_END<>;
pub fn parseString(parser: *Parser) ParseError!String {
    _ = try parseToken(parser, .string_start, null);
    var stringBits = std.ArrayList(String.StringBit).init(parser.arena);
    while (true) {
        const rtxt = parseToken(parser, .string, null) catch break;
        try stringBits.append(.{ .string = rtxt.text });
    }
    _ = parseToken(parser, .string_end, null) catch @panic("hard fail");
    return String{ .bits = stringBits.toOwnedSlice() };
}

/// magic = '#' identifier<name> '(' component[',']<args> ')';
pub fn parseMagic(parser: *Parser) ParseError!Magic {
    _ = try parseToken(parser, .punctuation, "#");

    const name = parseToken(parser, .identifier, null) catch @panic("hard fail");
    const allocated = try parser.arena.create(Identifier);
    allocated.* = .{ .name = name.text };

    var argsAL = std.ArrayList(Component).init(parser.arena);

    _ = parseToken(parser, .punctuation, "(") catch @panic("hard fail");
    while (true) {
        try argsAL.append(parseComponent(parser) catch break);
        _ = parseToken(parser, .punctuation, ",") catch break;
    }
    _ = parseToken(parser, .punctuation, ")") catch @panic("hard fail");

    return Magic{
        .name = allocated,
        .args = argsAL.toOwnedSlice(),
    };
}

/// nameset = '<' identifier?<name> '>';
pub fn parseNameset(parser: *Parser) ParseError!Nameset {
    _ = try parseToken(parser, .punctuation, "<");

    const rv: ?*Identifier = blk: {
        const v = parseToken(parser, .identifier, null) catch break :blk null;
        const allocated = try parser.arena.create(Identifier);
        allocated.* = .{ .name = v.text };
        break :blk allocated;
    };

    _ = parseToken(parser, .punctuation, ">") catch @panic("hard fail");

    return Nameset{ .name = rv };
}

/// array = '[' component? ']';
pub fn parseArray(parser: *Parser) ParseError!Array {
    _ = try parseToken(parser, .punctuation, "[");
    const component: ?*Component = blk: {
        const component = parseComponent(parser) catch break :blk null;
        const allocated = try parser.arena.create(@TypeOf(component));
        allocated.* = component;
        break :blk allocated;
    };
    _ = parseToken(parser, .punctuation, "]") catch @panic("hard fail");
    return Array{
        .component = component,
    };
}

/// optional = '?';
pub fn parseOptional(parser: *Parser) ParseError!Optional {
    _ = try parseToken(parser, .punctuation, "?");
    return Optional{};
}

pub fn parseToken(parser: *Parser, tokenKind: Token.Type, expectedText: ?[]const u8) ParseError!Token {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const tok = (try parser.nextToken()) orelse return parser.err("err");
    if (tok.kind != tokenKind) return parser.err("err");
    if (expectedText) |txt| if (!std.mem.eql(u8, tok.text, txt)) return parser.err("err");

    return tok;
}

test "demo" {
    const code = @embedFile("resyn.resyn");

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var parser = Parser.init(&arena.allocator, code);
    defer parser.deinit();

    const file = try parseFile(&parser);
    if ((try parser.nextToken())) |tok| {
        std.debug.panic("Remaining token: {}\n", .{tok});
    }

    const os = std.io.getStdOut().outStream();
    try os.writeAll("\n\n");
    try file.print(os);
    try os.writeAll("\n\n");
}
