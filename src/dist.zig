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

    const _9 = try parseHello(parser);
    const _9 = try parser.alloc.create(@TypeOf(_10));
    _10.* = _9;
    return _10;
}
fn parse_7() ParseError!_2 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return _11(parser) catch |e| switch (e) {
        error.OutOfMemory => return e,
        error.ParseError => return null, // note that the called function already cancelBit'd so it's ok
    };
}
fn parse_11() ParseError!_1 {
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
fn parse_15() ParseError!_14 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return try parseToken(parser, .punctuation, "hello");
}
pub const parseHello = _15;
pub const World = _16;
const _16 = []const u8;
fn parse_17() ParseError!_16 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);

    return try parseToken(parser, .punctuation, "world");
}
pub const parseWorld = _17;
