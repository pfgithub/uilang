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

const StructField = struct { field: ?[]const u8, structure: Structure };
// this needs to store an id or something
// so printing it can give a type name rather than a type
const StructureKind = union(enum) {
    empty,
    struc: struct {
        values: []StructField,
        // if values.len == 0, print struct {__: u1 = 0}
    },
    pointer: []const u8,
    token, // TODO token needs to be like []const u8 or smthn
    optional: *Structure,
};
const Structure = struct {
    name: ?[]const u8, // the name if it has one
    kind: StructureKind,
    typeNameID: usize,
    fn print(structure: Structure, out: anytype) @TypeOf(out).Error!void {
        try out.print("_{}", .{structure.typeNameID});
    }
    fn printDecl(structure: Structure, out: anytype) @TypeOf(out).Error!void {
        try out.print("const _{} = ", .{structure.typeNameID});
        switch (structure.kind) {
            .empty => try out.writeAll(
                \\struct {__: u1 = 0}
            ),
            .struc => |sct| {
                try out.writeAll("struct {");
                for (sct.values) |val| {
                    if (val.field == null) continue;
                    try out.writeAll(val.field.?);
                    try out.writeAll(": ");
                    try val.structure.print(out);
                    try out.writeAll(", "); // for zig fmt
                }
                // try out.writeAll("const parse = parse");
                // try writeTypeNameFor();
                // try out.writeAll(";");
                try out.writeAll("};\n");

                for (sct.values) |val| {
                    try val.structure.printDecl(out);
                }
            },
            .pointer => |ptr| {
                try out.writeAll("*");
                try writeTypeNameFor(out, ptr);
                try out.writeAll(";\n");
            },
            .token => try out.writeAll("[]const u8;\n"),
            .optional => |optv| {
                try out.writeAll("?");
                try optv.print(out);
                try out.writeAll(";\n");

                try optv.printDecl(out);
            },
        }
    }
    fn init(generator: *Generator, name: ?[]const u8, kind: StructureKind) Structure {
        return .{
            .typeNameID = generator.nextID(),
            .name = name,
            .kind = kind,
        };
    }
    fn createForComponent(alloc: *Alloc, component: parser.Component, gen: *Generator) OOM!Structure {
        switch (component) {
            .or_op => unreachable, // TODO make a union
            .p_op => |p_components| {
                // create component structures for each item
                // store any named structures into a struct
                // return the struct of named structures only
                // unnamed structures are ignored.
                var resFields = std.ArrayList(StructField).init(alloc);

                for (p_components) |p_component| {
                    const structure = try createForComponent(alloc, p_component, gen);

                    if (structure.name == null) continue; // unnamed results are not stored.
                    try resFields.append(.{ .field = structure.name, .structure = structure });
                }

                return Structure.init(gen, null, .{ .struc = .{ .values = resFields.toOwnedSlice() } });
            },
            .decl_ref => |dr| return Structure.init(gen, dr.name, .{ .pointer = dr.name }),
            .parens => unreachable, // TODO
            .string => |str| return Structure.init(gen, null, .token),
            .magic => unreachable, // TODO
            .suffixop => |sfxop| {
                const structure = try createForComponent(alloc, sfxop.component.*, gen);
                switch (sfxop.suffixop.*) {
                    .nameset => |ns| return Structure.init(gen, if (ns.name) |q| q.name else null, structure.kind),
                    .array => unreachable, // TODO :: note that if the seperator is named,
                    // the type may be []struct {seperatorname: , valuename: } instead of []value
                    .optional => {
                        const allocated = try alloc.create(Structure);
                        allocated.* = structure;
                        return Structure.init(gen, structure.name, .{ .optional = allocated });
                    },
                }
            },
        }
    }
};

// should each thing make its own parse fn?
// fn parse__25() ParseError!void { return try parseToken("(")}
pub fn codegenForStructure(alloc: *Alloc, generator: *Generator, structure: Structure, out: anytype, myid: usize) (@TypeOf(out).Error || OOM)!void {
    const fnName: usize = myid;
    try out.print("fn parse_{} () ParseError!_{}", .{ fnName, structure.typeNameID });
    try out.writeAll(" {\n");
    try out.writeAll(
        \\    const sb = parser.startBit();
        \\    errdefer parser.cancelBit(sb);
        \\
    );

    var nextCodegens = std.ArrayList(struct { structure: *Structure, fnid: usize }).init(alloc);

    switch (structure.kind) {
        .struc => |struc| {
            var resMap = std.ArrayList(struct { name: []const u8, id: usize }).init(alloc);
            for (struc.values) |*value| {
                const fnid = generator.nextID();
                if (value.field) |nme| {
                    const id = generator.nextID();
                    try out.print("    const _{} = ", .{id});
                    try resMap.append(.{ .name = nme, .id = id });
                } else try out.print("    _ = ", .{});
                try out.print("    try parse_{}(parser);\n", .{fnid});

                try nextCodegens.append(.{ .structure = &value.structure, .fnid = fnid });
            }

            try out.writeAll("    return ");
            try structure.print(out);
            try out.writeAll("{\n");
            for (resMap.items) |rv| {
                try out.print("        .{} = _{},\n", .{ rv.name, rv.id });
            }
            try out.writeAll("    };\n");

            // add .{value.structure, generator.nextID()} to the arraylist of codegens to do next
        },
        .pointer => |itmnme| {
            const resultid = generator.nextID();
            const allocid = generator.nextID();
            try out.print("    const _{} = try parse", .{resultid});
            try writeTypeNameFor(out, itmnme);
            try out.writeAll("(parser);\n");
            try out.print("    const _{} = try parser.alloc.create(@TypeOf(_{}));\n", .{ resultid, allocid });
            try out.print("    _{}.* = _{};\n", .{ allocid, resultid });
            try out.print("    return _{};\n", .{allocid});
        },
        else => try out.print("    @compileError(\"TODO: {}\");\n", .{std.meta.tagName(structure.kind)}),
    }
    try out.writeAll(
        \\}
        \\
    );

    for (nextCodegens.items) |cdgen| {
        try codegenForStructure(alloc, generator, cdgen.structure.*, out, cdgen.fnid);
    }
}

pub const Generator = struct {
    alloc: *Alloc,
    uniqueid: usize = 0,

    pub fn nextID(gen: *Generator) usize {
        defer gen.uniqueid += 1;
        return gen.uniqueid;
    }

    pub fn parse(alloc: *Alloc, code: []const u8) !void {
        var parserr = parser.Parser.init(alloc, code);
        defer parserr.deinit();

        const file = try parser.parseFile(&parserr);
        if ((try parserr.nextToken())) |tok| {
            std.debug.panic("Remaining token: {}\n", .{tok});
        }

        const os = std.io.getStdOut().outStream();

        try os.writeAll("\n\n");

        var gen = Generator{ .alloc = alloc };

        for (file.decls) |decl| {
            // create a structure for decl.value
            // decl.value
            const structure = try Structure.createForComponent(alloc, decl.value.*, &gen);
            // top level structures must be force wrapped into a struct

            try os.writeAll("pub const ");
            try writeTypeNameFor(os, decl.name);
            try os.writeAll(" = ");
            try structure.print(os);
            try os.writeAll(";\n");
            try structure.printDecl(os);

            const resid = gen.nextID();
            try codegenForStructure(alloc, &gen, structure, os, resid);
            try os.writeAll("pub const parse");
            try writeTypeNameFor(os, decl.name);
            try os.print(" = _{};\n", .{resid});
        }

        try os.writeAll("\n\n");
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

    try Generator.parse(&arena.allocator, code);
    // defer generator.deinit();

    const os = std.io.getStdOut().outStream();
    try os.writeAll("\n\n");
    // try generator.generate(os);
    try os.writeAll("\n\n");
}
