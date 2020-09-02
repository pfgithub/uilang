const std = @import("std");
const parser = @import("dist.zig"); // in the future this will be added from build.zig in an addPackagePath so you can @import("parser") or something

test "dist" {
    const code =
        \\a * a + a * a
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = &arena.allocator;

    const parsed = try parser.parse(alloc, code, .Math);

    std.debug.warn("\n\nParsed: {}\n\n", .{parsed});
}
