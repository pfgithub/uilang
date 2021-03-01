const ast = @import("uilang_parser");
const std = @import("std");
const Alloc = std.mem.Allocator;
const IR = @import("ir.zig").IR;
const Type = @import("type.zig").Type;
usingnamespace @import("../../help.zig");

pub fn main() !void {
    var arena_alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_alloc.deinit();
    const alloc = &arena_alloc.allocator;

    const sample = @embedFile("./demo.uil");

    const out = std.io.getStdOut().writer();

    try ast.printSyntaxHighlight(sample, out);

    try out.writeAll("\n\n------\n\n");

    const parsed = try ast.parse(alloc, sample, .File);

    // init a scope
    // generate ir for a code block

    const ir_block = IR.newBlock();
    const scope = Scope.new(alloc, null, ir_block, .{});
    for (parsed) |expr| {
        evaluateExpression(scope, parsed);
    }
}

// these expressions create a new scope:
// () eg (let a = int: 25, a)
// {} eg <blkname>{let a = int: 25; return<blkname> 25;}
// <…> eg <blkname>({return <blkname> 25;}) // uuh maybe <blkname> should just be a thing that goes before parens or curlies
pub fn evaluateExpression(scope: Scope, expr: ast.Expression) *UntypedIR {
    switch (expr.*) {
        else => {
            std.debug.panic("TODO expression", .{expr.assignop});
        },
    }
}

pub const TypedValue = struct {};

pub const IR = union(enum) {
    block: struct {
        code: std.ArrayList(*IR),
    },
};

// scopes are created:
// when any block opens {}
// when any parens open () I guess
// when any scope label is used
pub const Scope = struct {
    parent: ?*Scope,
    cf_catch: CFCatch,
    ir_block: *IR,
    name_map: NameMap,
    const CFCatch = union(enum) {
        ret,
        label: []const u8,
    };
    const NameMap = std.StringHashMap(*TypedValue);
    pub fn new(alloc: *std.mem.Allocator, parent: ?*Scope, cf_block: *IR, cf_catch: CFCatch) *Scope {
        var allocated = alloc.create(Scope) catch @panic("oom");

        allocated.* = .{
            .parent = parent,
            .cf_catch = cf_catch,
            .cf_block = cf_block,
            .name_map = NameMap.init(alloc),
        };

        return allocated;
    }
};

// <break>: while(true) <continue>: {
//     return<continue> 25;
//     return<break> 10;
// }

// <label>:
