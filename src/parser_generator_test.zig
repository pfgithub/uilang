const std = @import("std");
const parser = @import("resyn_parser");

fn printComponent(component: parser.Component, out: anytype) @TypeOf(out).Error!void {
    switch (component) {
        .or_op => |orop| for (orop) |cmpnt, i| {
            if (i != 0) try out.writeAll(" | ");
            try printComponent(cmpnt, out);
        },
        .p_op => |pop| for (pop) |cmpnt, i| {
            if (i != 0) try out.writeAll(" ");
            try printComponent(cmpnt, out);
        },
        .suffixop => |sfxop| {
            try printComponent(sfxop._.*, out);
            switch (sfxop.suffixop.*) {
                .nameset => |ns| {
                    try out.writeAll("<");
                    if (ns.name) |nme| try out.writeAll(nme);
                    try out.writeAll(">");
                },
                .array => |ary| {
                    try out.writeAll("[");
                    if (ary.component) |cmpnt| try printComponent(cmpnt.*, out);
                    try out.writeAll("]");
                },
                .optional => try out.writeAll("?"),
            }
        },
        .decl_ref => |dclref| {
            try out.writeAll(dclref);
        },
        .token_ref => |tknref| {
            try out.writeAll(":");
            try out.writeAll(tknref.token);
        },
        .parens => |itm| {
            try out.writeAll("(");
            try printComponent(itm.component.*, out);
            try out.writeAll(")");
        },
        .string => |stri| {
            try out.writeByte('"');
            for (stri.bits) |bit|
                switch (bit) {
                    .string => |txt| try out.writeAll(txt),
                    .escape => unreachable, // TODO string escapes
                };
            try out.writeByte('"');
        },
        .magic => |magi| {
            try out.writeAll("#");
            try out.writeAll(magi.name);
            try out.writeAll("(");
            for (magi.args) |a, i| {
                if (i != 0) try out.writeAll(", ");
                try printComponent(a, out);
            }
            try out.writeAll(")");
        },
        .force_struct => try out.writeAll("."),
    }
}

pub fn main() !void {
    const code = @embedFile("parser_generator/resyn.resyn");

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const parsed = try parser.parse(alloc, code, .File);

    var outAL = std.ArrayList(u8).init(alloc);
    const out = outAL.writer();

    for (parsed) |decl| {
        try out.writeAll(decl.name);
        try out.writeAll(" = ");
        try printComponent(decl.value.*, out);
        try out.writeAll(";\n");
    }

    const stdout = std.io.getStdOut().writer();

    try parser.printSyntaxHighlight(outAL.items, stdout);
}
