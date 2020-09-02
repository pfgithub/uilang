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
    @compileError("TODO: optional");
}
pub const parseMain = _4;
pub const Hello = _11;
const _11 = []const u8;
fn parse_12() ParseError!_11 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);
    @compileError("TODO: token");
}
pub const parseHello = _12;
pub const World = _13;
const _13 = []const u8;
fn parse_14() ParseError!_13 {
    const sb = parser.startBit();
    errdefer parser.cancelBit(sb);
    @compileError("TODO: token");
}
pub const parseWorld = _14;
