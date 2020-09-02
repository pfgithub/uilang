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
    token: []const u8,
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
                try out.writeAll("struct {\n");
                for (sct.values) |val| {
                    if (val.field == null) continue;
                    try out.writeAll("    ");
                    try out.writeAll(val.field.?);
                    try out.writeAll(": ");
                    try val.structure.print(out);
                    try out.writeAll(",\n");
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
            .string => |str| {
                const expctdTkn = try parseString(alloc, str.*);
                return Structure.init(gen, null, .{ .token = expctdTkn });
            },
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

fn parseString(alloc: *Alloc, string: parser.String) ![]const u8 {
    var res = std.ArrayList(u8).init(alloc);
    for (string.bits) |sb| {
        switch (sb) {
            .string => |str| try res.appendSlice(str),
            .escape => unreachable, // TODO
        }
    }
    return res.toOwnedSlice();
}

pub fn codegenForStructure(alloc: *Alloc, generator: *Generator, structure: Structure, out: anytype, myid: usize) (@TypeOf(out).Error || OOM)!void {
    const fnName: usize = myid;
    // could be neat:
    // try out.writeAll("/// ");
    // try structure.component.print(out);
    // try out.writeAll("\n");
    try out.print("fn _{}(parser: *Parser) ParseError!_{}", .{ fnName, structure.typeNameID });
    try out.writeAll(" {\n");
    try out.writeAll(
        \\    const sb = parser.startBit();
        \\    errdefer parser.cancelBit(sb);
        \\
        \\
    );

    var nextCodegens = std.ArrayList(struct { structure: *Structure, fnid: usize }).init(alloc);

    switch (structure.kind) {
        .struc => |struc| {
            var resMap = std.ArrayList(struct { name: []const u8, id: usize }).init(alloc);
            for (struc.values) |*value| {
                const fnid = generator.nextID();
                try nextCodegens.append(.{ .structure = &value.structure, .fnid = fnid });

                if (value.field) |nme| {
                    const id = generator.nextID();
                    try out.print("    const _{} = ", .{id});
                    try resMap.append(.{ .name = nme, .id = id });
                } else try out.print("    _ = ", .{});
                try out.print("try _{}(parser);\n", .{fnid});
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
            try out.print("    const _{} = try parser.alloc.create(@TypeOf(_{}));\n", .{ allocid, resultid });
            try out.print("    _{}.* = _{};\n", .{ allocid, resultid });
            try out.print("    return _{};\n", .{allocid});
        },
        .optional => |v| {
            const fnid = generator.nextID();
            try nextCodegens.append(.{ .structure = v, .fnid = fnid });

            try out.print("    return _{}(parser)", .{fnid});
            try out.writeAll(
                \\ catch |e| switch (e) {
                \\        error.OutOfMemory => return e,
                \\        error.ParseError => return null, // note that the called function already cancelBit'd so it's ok
                \\    };
                \\
            );
        },
        .token => |tktxt| {
            const kind: enum { punctuation, identifier } = for (tktxt) |char| switch (char) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => break .identifier,
                else => break .punctuation,
            }
            else .punctuation;
            try out.print("    return (try _parseToken(parser, .{}, ", .{std.meta.tagName(kind)});
            try printZigString(tktxt, out);
            try out.writeAll(")).text;\n");
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

pub fn printZigString(str: []const u8, out: anytype) !void {
    try out.writeByte('"');
    for (str) |char| switch (char) {
        ' '...'~' => try out.writeByte(char),
        '\n' => try out.writeAll("\\n"),
        else => try out.print("\\x{x:0<2}", .{char}),
    };
    try out.writeByte('"');
}

pub const Generator = struct {
    alloc: *Alloc,
    uniqueid: usize = 0,

    pub fn nextID(gen: *Generator) usize {
        defer gen.uniqueid += 1;
        return gen.uniqueid;
    }

    pub fn parse(alloc: *Alloc, code: []const u8, out: anytype) !void {
        var parserr = parser.Parser.init(alloc, code);
        defer parserr.deinit();

        const file = try parser.parseFile(&parserr);
        if ((try parserr.nextToken())) |tok| {
            std.debug.panic("Remaining token: {}\n", .{tok});
        }

        const os = std.io.getStdOut().outStream();

        try out.writeAll(
            \\//! Autogenerated Code.
            \\//! Manual edits may be overwritten on rebuild.
        ++ "\n\n");
        try out.writeAll(@embedFile("parser_header.zig"));
        // I would use ++ on embedFile but current zig makes that include the file twice, once without \n\n and once with it.
        try out.writeAll("\n\n");

        var gen = Generator{ .alloc = alloc };

        for (file.decls) |decl| {
            // create a structure for decl.value
            // decl.value
            const structure = try Structure.createForComponent(alloc, decl.value.*, &gen);
            // top level structures must be force wrapped into a struct

            try out.writeAll("pub const ");
            try writeTypeNameFor(os, decl.name);
            try out.writeAll(" = ");
            try structure.print(os);
            try out.writeAll(";\n");
            try structure.printDecl(os);

            const resid = gen.nextID();
            try codegenForStructure(alloc, &gen, structure, os, resid);
            try out.writeAll("pub const parse");
            try writeTypeNameFor(os, decl.name);
            try out.print(" = _{};\n", .{resid});
        }
    }

    pub fn deinit(generator: *Generator) void {}
};

pub fn main() !void {
    const code =
        \\ main = hello world?;
        \\ hello = "hello";
        \\ world = "world";
    ;

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const os = std.io.getStdOut().outStream();

    try Generator.parse(&arena.allocator, code, os);
}
