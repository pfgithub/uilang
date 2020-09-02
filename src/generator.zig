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
const UnionMagic = union(enum) {
    none: Structure,
    operator: Structure,
};
const UnionField = struct { field: []const u8, magic: UnionMagic };
// this needs to store an id or something
// so printing it can give a type name rather than a type
const StructureKind = union(enum) {
    unattached_magic: struct {
        name: []const u8,
        args: []parser.Component, // []StructureKind? it seems like []parser.Component may give more control but idk
    },
    unio: struct {
        values: []UnionField,
    },
    struc: struct {
        values: []StructField,
        // if values.len == 0, print struct {__: u1 = 0}
    },
    pointer: []const u8, // TODO maybe: make this *StructureKind.
    value: []const u8,
    token: []const u8,
    optional: *Structure,
    array_only: struct {
        item: *Structure,
        joiner: *Structure,
    },
};
const Structure = struct {
    name: ?[]const u8, // the name if it has one
    kind: StructureKind,
    typeNameID: usize,
    fn print(structure: Structure, out: anytype) @TypeOf(out).Error!void {
        try out.print("_{}", .{structure.typeNameID});
    }
    // what if we still recursively printed but we made it generate multiple decls
    // so eg: const DeclName = struct {hello: *Hello}
    // const _0 = DeclName;
    // const _1 = DeclName::hello::optionalunwrap ok yeah uuh
    // that could be neat right?
    // and then printDecl is for the top level and has an arg for DeclName
    // or maybe generate a _template_DeclName which can be looked at by humans
    fn printDecl(structure: Structure, out: anytype) @TypeOf(out).Error!void {
        try out.print("const _{} = ", .{structure.typeNameID});
        switch (structure.kind) {
            .unattached_magic => unreachable, // TODO report error unattached magic
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
                try out.writeAll("};\n");

                for (sct.values) |val| {
                    try val.structure.printDecl(out);
                }
            },
            .unio => |sct| {
                try out.writeAll("union(enum) {\n");
                for (sct.values) |val| {
                    try out.writeAll("    ");
                    try out.writeAll(val.field);
                    try out.writeAll(": ");
                    switch (val.magic) {
                        .none => |base| try base.print(out),
                        .operator => try out.print("[]_{}", .{structure.typeNameID}),
                    }
                    try out.writeAll(",\n");
                }
                try out.writeAll("};\n");

                for (sct.values) |val| {
                    switch (val.magic) {
                        .none => |base| try base.printDecl(out),
                        .operator => |base| try base.printDecl(out),
                    }
                }
            },
            .pointer => |ptr| {
                try out.writeAll("*");
                try writeTypeNameFor(out, ptr);
                try out.writeAll(";\n");
            },
            .value => |valu| {
                try writeTypeNameFor(out, valu);
                try out.writeAll(";\n");
            },
            .token => try out.writeAll("[]const u8;\n"),
            .optional => |optv| {
                try out.writeAll("?");
                try optv.print(out);
                try out.writeAll(";\n");

                try optv.printDecl(out);
            },
            .array_only => |ao| {
                try out.writeAll("[]"); // []const ?
                try ao.item.print(out);
                try out.writeAll(";\n");

                try ao.item.printDecl(out);
                try ao.joiner.printDecl(out);
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
            .or_op => |or_components| {
                var resFields = std.ArrayList(UnionField).init(alloc);

                for (or_components) |or_component| {
                    const structure = try createForComponent(alloc, or_component, gen);
                    // if magic::
                    // wrap the final union

                    if (structure.name == null) unreachable; // TODO report error :: all union fields must be named

                    // this would be better if(structure.kind.unattached_magic) |uam| but zig will never have that for a few reasons
                    // maybe it would be better if union and union(enum) weren't so similar. idk. what if enum was union(enum) and union was just for union
                    // that might even make some things more clear in the language. probably not worth the change idk. it is simpler though maybe not really
                    // not really
                    switch (structure.kind) {
                        .unattached_magic => |uam| {
                            const uamName = std.meta.stringToEnum(enum { operator }, uam.name) orelse unreachable; // TODO report error :: invalid magic name
                            // uam.name, uam.args
                            switch (uamName) {
                                .operator => {
                                    // aaa the indentation is too much help pls
                                    if (uam.args.len != 1) unreachable; // TODO report error :: invalid number of args
                                    const uamJoiner = try createForComponent(alloc, uam.args[0], gen);
                                    if (uamJoiner.name != null) unreachable; // TODO support named operators
                                    // ok so structure is actually = to []ThisUnion
                                    // which is weird
                                    // maybe it should have no structure then
                                    try resFields.append(.{
                                        .field = structure.name.?,
                                        .magic = .{ .operator = uamJoiner },
                                    });
                                },
                            }
                        },
                        else => try resFields.append(.{
                            .field = structure.name.?,
                            .magic = .{ .none = structure },
                        }),
                    }
                }

                return Structure.init(gen, null, .{ .unio = .{ .values = resFields.toOwnedSlice() } });
            },
            .p_op => |p_components| {
                var resFields = std.ArrayList(StructField).init(alloc);

                for (p_components) |p_component| {
                    const structure = try createForComponent(alloc, p_component, gen);

                    try resFields.append(.{ .field = structure.name, .structure = structure });
                }

                return Structure.init(gen, null, .{ .struc = .{ .values = resFields.toOwnedSlice() } });
            },
            .decl_ref => |dr| return Structure.init(gen, dr.name, .{ .pointer = dr.name }),
            .parens => |pr| return createForComponent(alloc, pr.component.*, gen),
            .string => |str| {
                const expctdTkn = try parseString(alloc, str.*);
                return Structure.init(gen, null, .{ .token = expctdTkn });
            },
            .magic => |magic| return Structure.init(gen, null, .{
                .unattached_magic = .{ .name = magic.name.name, .args = magic.args },
            }),
            .suffixop => |sfxop| {
                const structure = try createForComponent(alloc, sfxop.component.*, gen);
                switch (sfxop.suffixop.*) {
                    .nameset => |ns| return Structure.init(gen, if (ns.name) |q| q.name else null, structure.kind),
                    .array => |ary| {
                        if (ary.component == null) unreachable; // TODO support arrays without joiners
                        const joiner = try createForComponent(alloc, ary.component.?.*, gen);
                        if (joiner.name != null) unreachable; // TODO support named joiners
                        const allocatedStructure = switch (structure.kind) {
                            .pointer => |pname| try allocDupe(alloc, Structure.init(gen, structure.name, .{ .value = pname })),
                            else => try allocDupe(alloc, structure),
                        };
                        const allocatedJoiner = try allocDupe(alloc, joiner);
                        return Structure.init(gen, null, .{ .array_only = .{ .item = allocatedStructure, .joiner = allocatedJoiner } });
                    },
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

fn allocDupe(alloc: *Alloc, a: anytype) !*@TypeOf(a) {
    const c = try alloc.create(@TypeOf(a));
    c.* = a;
    return c;
}

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
        .unattached_magic => unreachable, // TODO report error unattached magic
        .unio => |unio| {
            for (unio.values) |*value| {
                switch (value.magic) {
                    .none => |*nost| {
                        const fnid = generator.nextID();
                        try nextCodegens.append(.{ .structure = nost, .fnid = fnid });

                        try out.print(
                            \\    blk: {{
                            \\        return _{}{{ .{} = _{}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break :blk}} }};
                            \\    }}
                        ,
                            .{ structure.typeNameID, value.field, fnid },
                        );
                    },
                    .operator => |*nost| {
                        const joinerFnID = generator.nextID();
                        try nextCodegens.append(.{ .structure = nost, .fnid = joinerFnID });
                        const nextFunctionHalf = generator.nextID();

                        // imagine: automatically detect
                        // a = a '+' a | number
                        // that would be doable but bad idk
                        // anyway it would mean a would actually refer to the remainder of the union
                        // but with how this is currently programmed, that would be a mess

                        // oh my. is there a way to have named fields?
                        try out.print(
                            \\    var resAL = std.ArrayList(_{0}).init(parser.alloc);
                            \\    _ = _{1}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => {{}}}}; // optional first joiner
                            \\    while (true) {{
                            \\        try resAL.append(_{2}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => return parser.err("last or disallowed")}});
                            \\        _ = _{1}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break}};
                            \\    }}
                            \\    if (resAL.items.len == 0) return parser.err("no items");
                            \\    if (resAL.items.len == 1) return resAL.items[0];
                            \\
                            \\    return _{0}{{ .{3} = resAL.toOwnedSlice() }};
                            \\}}
                            \\fn _{2}(parser: *Parser) ParseError!_{0} {{
                            \\    const sb = parser.startBit();
                            \\    errdefer parser.cancelBit(sb);
                            \\
                        ,
                            .{
                                structure.typeNameID,
                                joinerFnID,
                                nextFunctionHalf,
                                value.field,
                            },
                        );
                    },
                }
            }
            try out.writeAll("\n    return parser.err(\"union field not matched f\");");
        },
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
        .value => |itmnme| {
            const resultid = generator.nextID();
            const allocid = generator.nextID();
            try out.print("    return try parse", .{});
            try writeTypeNameFor(out, itmnme);
            try out.writeAll("(parser);\n");
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
        .array_only => |ao| {
            // item, joiner
            const itemFnid = generator.nextID();
            try nextCodegens.append(.{ .structure = ao.item, .fnid = itemFnid });
            const joinerFnid = generator.nextID();
            try nextCodegens.append(.{ .structure = ao.joiner, .fnid = joinerFnid });
            @setEvalBranchQuota(1010100);
            try out.print(
                \\    var resAL = std.ArrayList(_{}).init(parser.alloc);
                \\    while(true) {{
                \\        // :: parse 1 catch break
                \\        try resAL.append(_{}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break}});
                \\        // :: parse 2 catch break
                \\        _ = _{}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break}};
                \\    }}
                \\    return resAL.toOwnedSlice();
                \\
            , .{ ao.item.typeNameID, itemFnid, joinerFnid });
        },
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
        \\ math = 
        \\    | #operator("+")<plus_op>
        \\    | #operator("*")<times_op>
        \\    | parens
        \\    | number
        \\ ;
        \\ parens = '(' math ')';
        \\ number = 'a';
    ;

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const os = std.io.getStdOut().outStream();

    try Generator.parse(&arena.allocator, code, os);
}
