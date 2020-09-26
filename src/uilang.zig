const std = @import("std");
const ast = @import("uilang_parser");
const Alloc = std.mem.Allocator;

const astgen = @import("astgen.zig");
const codegen = @import("codegen.zig");

pub fn main() !void {
    const code = @embedFile("../tests/consistent2.ul");

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const parsed = try ast.parse(alloc, code, .File);

    const out = std.io.getStdOut().writer();

    const resvir = try astgen.astToIR(alloc, parsed);
    try codegen.printRoot(resvir, out);
}

test "" {
    std.meta.refAllDecls(@This());
}
