// example of a function parsing a .pointer
fn _0(parser: *Parser) ParseError!*Hello {
    const _6 = try parseHello(parser);
    const _7 = try parser.alloc.create(@TypeOf(_6));
    _7.* = _6;
    return _7;
}

// example of a function parsing a .optional
fn _1(parser: *Parser) ParseError!?*World {
    return _5() catch |e| switch (e) {
        error.OutOfMemory => return e,
        error.ParseError => return null,
    };
}

// example of a function parsing a .pointer
fn _5(parser: *Parser) ParseError!*World {
    const _8 = try parseHello(parser);
    const _9 = try parser.alloc.create(@TypeOf(_8));
    _9.* = _8;
    return _9;
}

// example of a function parsing a .struc
pub fn parseX(parser: *Parser) ParseError!X {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    const _2 = try _0(parser);
    const _3 = try _1(parser);

    return Parens{ .component = allocatedComponent };
}

pub const Main = _3;
const _3 = struct {
    hello: _0,
    world: _2,
};
const _0 = *Hello;
const _2 = ?_1;
const _1 = *World;
fn parse_4() ParseError!_3 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);
    const _6 = try parse_5(parser);
    const _8 = try parse_7(parser);
    return _3{
        .hello = _6,
        .world = _8,
    };
}
fn parse_5() ParseError!_0 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);
    @compileError("TODO: pointer");
}
fn parse_7() ParseError!_2 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);
    @compileError("TODO: optional");
}
pub const parseMain = _4;
pub const Hello = _9;
const _9 = []const u8;
fn parse_10() ParseError!_9 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);
    @compileError("TODO: token");
}
pub const parseHello = _10;
pub const World = _11;
const _11 = []const u8;
fn parse_12() ParseError!_11 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);
    @compileError("TODO: token");
}
pub const parseWorld = _12;
