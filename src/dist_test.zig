const std = @import("std");
const parser = @import("dist.zig"); // in the future this will be added from build.zig in an addPackagePath so you can @import("parser") or something

fn printMath(math: parser.Math, out: anytype) @TypeOf(out).Error!void {
    switch (math) {
        .plus_op => |plsop| for (plsop) |mth, i| {
            if (i != 0) try out.writeAll(" + ");
            try printMath(mth, out);
        },
        .times_op => |tmsop| for (tmsop) |mth, i| {
            if (i != 0) try out.writeAll("*");
            try printMath(mth, out);
        },
        .factorial => |sfxop| {
            try printMath(sfxop._.*, out);
            try out.writeAll("!");
        },
        .parens => |prns| try printMath(prns.math.*, out),
        .number => try out.writeAll("a"),
    }
}

pub fn main() !void {
    const code =
        \\a * a! + a * a!
    ;

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const parsed = try parser.parse(alloc, code, .Math);

    const out = std.io.getStdOut().writer();

    try printMath(parsed, out);
}
