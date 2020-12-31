const ast = @import("uilang_parser");
const std = @import("std");
const Alloc = std.mem.Allocator;
const IR = @import("ir.zig").IR;
const Type = @import("type.zig").Type;
usingnamespace @import("../help.zig");

pub fn main() !void {
    const sample = @embedFile("sample.uil");

    var arena_alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_alloc.deinit();

    const alloc = &arena_alloc.allocator;

    const parsed = try ast.parse(alloc, sample, .File);

    const out = std.io.getStdOut().writer();
    try printAst(parsed, out, IndentWriter{});
}

fn printAst(node: anytype, out: anytype, indent: IndentWriter) @TypeOf(out).Error!void {
    // inline switch #7224
    switch (@TypeOf(node)) {
        ast.File => for (node) |decl| {
            try out.print("{}", .{indent});
            try printAst(decl, out, indent);
            try out.writeAll(";\n");
        },
        ast.Decl => switch (node) {
            .pubvariable => |pvd| {
                try out.writeAll("pub ");
                try printAst(pvd.vardecl.*, out, indent);
            },
            .vardecl => |vd| {
                try printAst(vd.*, out, indent);
            },
        },
        ast.Vardecl => {
            try out.print("{} {} = ", .{ std.meta.tagName(node.vartype), node.name.* });
            try printAst(node.initv.*, out, indent);
        },
        ast.Expression => {
            switch (node) {
                .function => |n| try printAst(n.*, out, indent),
                else => try out.print("@todo(.{})", .{std.meta.tagName(node)}),
            }
        },
        ast.Function => {
            try out.print("{} (", .{std.meta.tagName(node.kind)});
            for (node.args) |arg, i| {
                if (i != 0) try out.writeAll(", ");
                try out.print("{}", .{arg});
            }
            try out.writeAll(") ");
            try printAst(node.expression.*, out, indent);
        },
        ast.Block => {},
        else => try out.writeAll("@todo(" ++ @typeName(@TypeOf(node)) ++ ")"),
    }
}

const IndentWriter = struct {
    count: usize = 0,
    pub fn format(idw: IndentWriter, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        for (range(idw.count)) |_| {
            try out.writeAll("    ");
        }
    }
    pub fn add(a: IndentWriter, b: usize) IndentWriter {
        return IndentWriter{ .count = a.count + b };
    }
};
