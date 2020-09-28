const std = @import("std");
const IR = @import("ir.zig").IR;
const Alloc = std.mem.Allocator;
usingnamespace @import("help.zig");

//! ir → js

// what if we just defined simple exprs as
// pub const unwatch = "({lhs}).value";
// that would be easier

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
        // .assign => |an| return requiresBlock(an.lval.*) or requiresBlock(ar.rval.*),
        .assign => return true, // make things easier for now.
        .math => return true, // it needs to be easier to make these print things
        .unwatch => return true, // ^
        else => return false,
    }
}

const SimpleExprs = struct {
    number = "{#.num}",
    string = "{'.str}",
    varget = "{@.vnm}",
    attr = "ō.attr({'.name}, {.value})",
};

// checks if any children require block printing
fn printUnified(ir: IR, out: anytype, indent: usize, return_blkid: bool, allow_block: bool) @TypeOf(out).Error!void {
    const idnt = IndentWriter{ .count = indent };
    // const out_left = OutLeftWriter{.outvar = };
    switch (ir) {
        .number => |num| try out.print("{}{}{}", .{ out_left, num, out_right }),
        .string => |str| try out.print("{}{}{}", .{ out_left, printString(str), out_right }),
        .varget => |vnm| try out.writeAll(vnm),
        .attr => |atr| {
            const atrv = try printAuto(atr.value, out, indent, return_blkid, allow_block);
            try out.print("{}ō.attr({}, {}){}", .{ out_left, printString(atr.name), atrv, out_right });
        },
        .assign => |av| {
            // lhs's last layer must be inline (if !watchable) otherwise this obviously
            // won't work. there should be some way to assert that
            const lhs = try printAuto(av.lval, out, indent, return_blkid, allow_block);
            const rhs = try printAuto(av.rhs, out, indent, return_blkid, allow_block);
            switch (av.watchable) { // hmm…
                true => try out.print("{}({}).set({}){}", .{ out_left, lhs, rhs, out_right }),
                false => try out.print("{}(({} = {}), undefined){}", .{ out_left, lhs, rhs, out_right }),
            }
        },
        // .html can still be done without allocation. just construct printAuto return values oh uuh hmm idk
        // it would be easy if a bit of information could be stored with the ir
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
                try out.print("{}", .{idnt});
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
        else => {
            try out.print("ō.TODO(\"{}\")", .{@tagName(ir)});
        },
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
            const attrv = try printAuto(ar.value, out, indent, return_blkid);
            if (write_to) |wt| {
                try out.print("{}var _{}_ = ō.attr(", .{ idnt, wt });
                try printJSString(ar.name, out);
                try out.print(", {});\n", .{attrv});
            }
        },
        .math => |mth| {
            const lhs = try printAuto(mth.lhs, out, indent, return_blkid);
            const rhs = try printAuto(mth.rhs, out, indent, return_blkid);
            if (write_to) |wt| {
                // note: with printAuto, this doesn't work right and might skip
                // necessary evaluations but whatever it doesn't matter rn.
                // todo refactor codegen to be better and not incorrect.
                try out.print("{}var _{}_ = {} ", .{ idnt, wt, lhs });
                switch (mth.op) {
                    .add => try out.writeAll("+"),
                    .sub => try out.writeAll("-"),
                    .mul => try out.writeAll("*"),
                    .div => try out.writeAll("/"),
                }
                try out.print(" {};\n", .{rhs});
            }
        },
        .assign => |asn| {
            // assign: struct { lval: *IR, rhs: *IR, watchable: bool },
            const lhs = try printAuto(asn.lval, out, indent, return_blkid);
            const rhs = try printAuto(asn.rhs, out, indent, return_blkid);

            if (asn.watchable) try out.print("{}({}).set({});\n", .{ idnt, lhs, rhs }) //
            else try out.print("{}{} = {};\n", .{ idnt, lhs, rhs });

            if (write_to) |wt| try out.print("{}var _{}_ = undefined;\n", .{ idnt, wt });
        },
        .unwatch => |uw| {
            const uwu = try printAuto(uw, out, indent, return_blkid);

            if (write_to) |wt| try out.print("{}var _{}_ = ({}).value;\n", .{ idnt, wt, uwu });
        },
        else => unreachable, // caught above
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
fn printString(str: []const u8) StringPrinter {
    return .{ .string = str };
}
const StringPrinter = struct {
    string: []const u8,
    pub fn format(spr: StringPrinter, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        try printJSString(spr.string, out);
    }
};

const IndentWriter = struct {
    count: usize,
    pub fn format(idw: IndentWriter, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        for (range(idw.count)) |_| {
            try out.writeAll("    ");
        }
    }
};
