const std = @import("std");
const Alloc = std.mem.Allocator;
const ast = @import("ast.zig");
const parser = @import("parser.zig");

const OOM = error{OutOfMemory};

// todo: keep a type name map thing and make sure the same type name doesn't get used twice
fn writeTypeNameFor(out: anytype, snakecase: []const u8) !void {
    var caps = true;
    for (snakecase) |char| {
        if (char == '_') {
            caps = true;
            continue;
        }
        if (caps) {
            if (char >= 'a' and char <= 'z') try out.writeByte(char - ('a' - 'A'))
            //a
            else try out.writeByte(char);
            caps = false;
            continue;
        }
        try out.writeByte(char);
    }
}

const StructField = struct { field: []const u8, structure: Structure };
const StructureKind = union(enum) {
    empty,
    struc: struct {
        values: []StructField,
        // if values.len == 0, print struct {__: u1 = 0}
    },
    pointer: []const u8,
    token,
    optional: *StructureKind,
    fn print(structure: StructureKind, out: anytype) @TypeOf(out).Error!void {
        switch (structure) {
            .empty => try out.writeAll(
                \\struct {__: u1 = 0}
            ),
            .struc => |sct| {
                try out.writeAll("struct {");
                for (sct.values) |val| {
                    try out.writeAll(val.field);
                    try out.writeAll(": ");
                    try val.structure.kind.print(out);
                    try out.writeAll(", "); // for zig fmt
                }
                try out.writeAll("}");
            },
            .pointer => |ptr| {
                try out.writeAll("*");
                try writeTypeNameFor(out, ptr);
            },
            .token => try out.writeAll("[]const u8"),
            .optional => |optv| {
                try out.writeAll("?");
                try optv.print(out);
            },
        }
    }
};
const Structure = struct {
    name: ?[]const u8, // the name if it has one
    kind: StructureKind,
    fn createForComponent(alloc: *Alloc, component: parser.Component) OOM!Structure {
        switch (component) {
            .or_op => unreachable, // TODO make a union
            .p_op => |p_components| {
                // create component structures for each item
                // store any named structures into a struct
                // return the struct of named structures only
                // unnamed structures are ignored.
                var resFields = std.ArrayList(StructField).init(alloc);
                for (p_components) |p_component| {
                    const structure = try createForComponent(alloc, p_component);
                    if (structure.name == null) continue; // unnamed results are not stored.
                    try resFields.append(.{ .field = structure.name.?, .structure = structure });
                }
                return Structure{
                    .name = null,
                    .kind = .{ .struc = .{ .values = resFields.toOwnedSlice() } },
                };
            },
            .decl_ref => |dr| {
                return Structure{
                    .name = dr.name,
                    .kind = .{ .pointer = dr.name },
                };
            },
            .parens => unreachable, // TODO
            .string => |str| return Structure{
                .name = null,
                .kind = .token,
            },
            .magic => unreachable, // TODO
            .suffixop => |sfxop| {
                const structure = try createForComponent(alloc, sfxop.component.*);
                switch (sfxop.suffixop.*) {
                    .nameset => |ns| return Structure{
                        .name = if (ns.name) |nme| nme.name else null,
                        .kind = structure.kind,
                    },
                    .array => unreachable, // TODO :: note that if the seperator is named, the type may be []struct {seperatorname: , valuename: } instead of []value
                    .optional => {
                        const allocated = try alloc.create(StructureKind);
                        allocated.* = structure.kind;
                        return Structure{
                            .name = structure.name,
                            .kind = .{ .optional = allocated },
                        };
                    }, // TODO
                }
            },
        }
    }
};

pub const Generator = struct {
    alloc: *Alloc,

    pub fn parse(alloc: *Alloc, code: []const u8) !Generator {
        var parserr = parser.Parser.init(alloc, code);
        defer parserr.deinit();

        const file = try parser.parseFile(&parserr);
        if ((try parserr.nextToken())) |tok| {
            std.debug.panic("Remaining token: {}\n", .{tok});
        }

        const os = std.io.getStdOut().outStream();

        try os.writeAll("\n\n");

        for (file.decls) |decl| {
            // create a structure for decl.value
            // decl.value
            const structure = try Structure.createForComponent(alloc, decl.value.*);
            // top level structures must be force wrapped into a struct

            try os.writeAll("pub const ");
            try writeTypeNameFor(os, decl.name);
            try os.writeAll(" = ");
            try structure.kind.print(os);
            try os.writeAll(";\n");
        }

        try os.writeAll("\n\n");

        return Generator{
            .alloc = alloc,
        };
    }

    pub fn deinit(generator: *Generator) void {}
};

test "generator" {
    const code =
        \\ main = hello world?;
        \\ hello = "hello";
        \\ world = "world";
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var generator = try Generator.parse(&arena.allocator, code);
    defer generator.deinit();

    const os = std.io.getStdOut().outStream();
    try os.writeAll("\n\n");
    // try generator.generate(os);
    try os.writeAll("\n\n");
}
