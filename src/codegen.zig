const std = @import("std");
const IR = @import("ir.zig").IR;
const Alloc = std.mem.Allocator;
usingnamespace @import("help.zig");

//! ir → js

fn requiresBlock(ir: IR) bool {
    switch (ir) {
        .block => |blk| {
            // TODO: loop over each item
            // if any of the items require a block, return true
            // excl the last item if it is a break from this block
            return true;
        },
        .vardecl => return true,
        .vardecl_w => return true,
        .number => return false,
        .string => return false,
        .varget => return false,
        .func => return false,
        .breakv => return true, // excl specific situations in blocks
        .html => |hv| {
            for (hv.args) |arg| if (requiresBlock(arg)) return true;
            return false;
        },
        .attr => |ar| {
            if (requiresBlock(ar.value.*)) return true;
            return false;
        },
        else => std.debug.panic("unsupported {}", .{@tagName(ir)}),
    }
}
fn printValue(ir: IR, out: anytype, indent: usize) @TypeOf(out).Error!void {
    const idnt = IndentWriter{ .count = indent };

    switch (ir) {
        .block => |blk| {
            // TODO: support parenthesis in specific situations
            unreachable; // requiresBlock returns false in this situation
        },
        .vardecl, .vardecl_w, .breakv => unreachable, // requiresBlock returns false in this situation
        .number => |num| try out.print("{}", .{num}),
        .string => |str| try printJSString(str, out),
        .varget => |vnm| try out.writeAll(vnm),
        .func => |func| {
            try out.writeAll("() => ");
            if (requiresBlock(func.body.*)) {
                try out.writeAll("{\n");
                switch (func.body.*) {
                    .block => |blk| for (blk.body) |blkir| {
                        _ = try print(blkir, out, indent + 1, null, blk.blockid orelse std.math.maxInt(usize));
                    },
                    else => {
                        const bodyresid = getNewID();
                        try print(func.body.*, out, indent + 1, bodyresid, std.math.maxInt(usize));
                        try out.print("{}    return _{}_;\n", .{ idnt, bodyresid });
                    },
                }
                try out.writeAll("}");
            } else {
                try printValue(func.body.*, out, indent);
            }
        },
        .html => |hl| {
            try out.writeAll("ō.html(");
            try printJSString(hl.tag, out);
            for (hl.args) |arg| {
                try out.print(",\n{}    ", .{idnt});
                try printValue(arg, out, indent + 1);
            }
            try out.print("\n{})", .{idnt});
        },
        .attr => |ar| {
            try out.writeAll("ō.attr(");
            try printJSString(ar.name, out);
            try out.writeAll(", ");
            try printValue(ar.value.*, out, indent);
            try out.writeAll(")");
        },
        else => std.debug.panic("unsupported {}", .{@tagName(ir)}),
    }
}
const AutoPrintResult = union(enum) {
    variable: usize,
    inlined: struct {
        ir: *const IR,
        indent: usize,
    },
    pub fn format(apr: AutoPrintResult, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        switch (apr) {
            .variable => |vnum| try out.print("_{}_", .{vnum}),
            .inlined => |ild| try printValue(ild.ir.*, out, ild.indent),
        }
    }
};
fn printAuto(ir: *const IR, out: anytype, indent: usize, return_blkid: usize) @TypeOf(out).Error!AutoPrintResult {
    if (requiresBlock(ir.*)) {
        const write_to = getNewID();
        try print(ir.*, out, indent, write_to, return_blkid);
        return AutoPrintResult{ .variable = write_to };
    }
    return AutoPrintResult{ .inlined = .{ .ir = ir, .indent = indent } };
}
pub fn printRoot(root_ir: IR, out: anytype) @TypeOf(out).Error!void {
    try print(root_ir, out, 0, null, std.math.maxInt(usize));
}
fn print(ir: IR, out: anytype, indent: usize, write_to: ?usize, return_blkid: usize) @TypeOf(out).Error!void {
    const idnt = IndentWriter{ .count = indent };

    if (!requiresBlock(ir)) {
        if (write_to) |wtid| try out.print("{}var _{}_ = ", .{ idnt, wtid }) //
        else try out.print("{}", .{idnt});
        try printValue(ir, out, indent);
        try out.writeAll(";\n");
        return;
    }

    switch (ir) {
        // .vardecl => |vd| {
        //     // TODO change this to only work inside blocks
        //     // otherwise expressions shouldn't end with ;
        //     try out.print("const {} = ", .{vd.jsname});
        //     try vd.initial.print(out, indent);
        //     try out.writeAll(";");
        // },
        .block => |blk| {
            if (blk.blockid) |blkid| {
                try out.print("{}var _{}_ = undefined;\n", .{ idnt, blkid });
                try out.print("{}_{}_blk_: {{\n", .{ idnt, blkid });
            }
            const nidnt = if (blk.blockid) |_| indent + 1 else indent;
            for (blk.body) |blkir| {
                _ = try print(blkir, out, nidnt, null, return_blkid);
            }
            if (blk.blockid) |_| try out.print("{}}}\n", .{idnt});
            if (write_to) |wt| {
                if (blk.blockid) |blkid| try out.print("{}var _{}_ = _{}_;\n", .{ idnt, wt, blkid }) // zig fmt
                else try out.print("{}var _{}_ = undefined;\n", .{ idnt, wt });
            }
        },
        .vardecl => |vd| {
            const printed = try printAuto(vd.initial, out, indent, return_blkid);
            try out.print("{}var {} = {};\n", .{ idnt, vd.jsname, printed });
            if (write_to) |wt| try out.print("{}var _{}_ = undefined;\n", .{ idnt, wt });
        },
        .vardecl_w => |vd| {
            const printed = try printAuto(vd.initial, out, indent, return_blkid);
            try out.print("{}var {} = ō.watchable({});\n", .{ idnt, vd.jsname, printed });
            if (write_to) |wt| try out.print("{}var _{}_ = undefined;\n", .{ idnt, wt });
        },
        .number, .string, .varget, .func => unreachable, // caught in requiresBlock
        .breakv => |bv| {
            const wtv = try printAuto(bv.value, out, indent, return_blkid);
            if (bv.blkid == return_blkid) {
                try out.print("{}return {};\n", .{ idnt, wtv });
            } else {
                try out.print("{}_{}_ = {};\n", .{ idnt, bv.blkid, wtv });
                try out.print("{}break _{}_blk;\n", .{ idnt, bv.blkid });
            }
        },
        .html => |hl| {
            // it might be worth it to just do an allocator here
            // and then also it will be easier because instead
            // of this id mess we can just make an arraylist
            // of AutoPrintResults and use those.:w
            var startID = global_id;
            for (range(hl.args.len)) |_| global_id += 1;
            for (hl.args) |hlarg, i| {
                const hlID = startID + i;
                try print(hlarg, out, indent, hlID, return_blkid);
            }
            if (write_to) |wt| {
                try out.print("{}var _{}_ = ō.html(", .{ idnt, wt });
                try printJSString(hl.tag, out);
                for (range(hl.args.len)) |_, i| {
                    const rid = startID + i;
                    try out.writeAll(", ");
                    try out.print("_{}_", .{rid});
                }
                try out.writeAll(");\n");
            }
        },
        .attr => |ar| {
            var arid = getNewID();
            try print(ar.value.*, out, indent, arid, return_blkid);
            if (write_to) |wt| {
                try out.print("{}var _{}_ = ō.attr(", .{ idnt, arid });
                try printJSString(ar.name, out);
                try out.print(", _{});\n", .{arid});
            }
        },
        else => {
            try out.print("{}", .{idnt});
            if (write_to) |wt| try out.print("var _{}_ = ", .{wt});
            try out.print("ō.TODO(\"{}\");\n", .{@tagName(ir)});
        },
    }
}

fn printJSString(str: []const u8, out: anytype) !void {
    try out.writeByte('"');
    for (str) |char| switch (char) {
        ' '...'~' => try out.writeByte(char),
        '\n' => try out.writeAll("\\n"),
        else => try out.print("\\x{x:0<2}", .{char}),
    };
    try out.writeByte('"');
}

const IndentWriter = struct {
    count: usize,
    pub fn format(idw: IndentWriter, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        for (range(idw.count)) |_| {
            try out.writeAll("    ");
        }
    }
};
