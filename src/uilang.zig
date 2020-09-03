const std = @import("std");
const parser = @import("uilang_parser");

pub fn main() !void {
    const code = @embedFile("../tests/consistent.ul");

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const parsed = try parser.parse(alloc, code, .File);

    const out = std.io.getStdOut().writer();

    for (parsed) |decl| {
        try out.print("decl: {}\n", .{decl});
    }
}
