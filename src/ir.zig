const std = @import("std.zig");
const Type = @import("type.zig").Type;
const Alloc = std.mem.Allocator;

//! ir

pub const MathOp = enum { add, sub, mul, div };
pub const IR = union(enum) {
    vardecl: struct {
        reassign: bool,
        initial: *IR,
        jsname: []const u8,
    },
    vardecl_w: struct {
        initial: *IR,
        jsname: []const u8,
    },
    varget: []const u8,
    unwatch: *IR, // $watchable.value
    math: struct { lhs: *IR, rhs: *IR, op: MathOp },
    assign: struct { lval: *IR, rhs: *IR, watchable: bool },
    html: struct { tag: []const u8, args: []IR },
    attr: struct { name: []const u8, value: *IR },
    breakv: struct { value: *IR, blkid: usize },
    block: struct { blockid: ?usize, body: []IR },
    func: struct {
        body: *IR,
    },
    number: []const u8,
    string: []const u8,
    watchable: struct {
        dependencies: []IR, // struct{dependenc: IR, mapnme: []const u8}?
        value: *IR,
    },
    htmlelement,
    htmlattribute,
    t_type: Type, // only available at comptime
};
