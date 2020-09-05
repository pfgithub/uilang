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
    operator: ?Structure,
    suffix: Structure,
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
    token: struct { kind: []const u8, expected: ?[]const u8 },
    optional: *Structure,
    array_only: struct {
        item: *Structure,
        joiner: ?*Structure,
    },
};
const PrintMode = enum { required, userdisplay, zigonly };
const Structure = struct {
    name: ?[]const u8, // the name if it has one
    kind: StructureKind,
    typeNameID: usize,
    // TODO there needs to be a way of differentiating required and not required sections. in not required sections, don't do this blk: thing at all.
    fn print(structure: Structure, out: anytype, mode: PrintMode) @TypeOf(out).Error!void {
        // try out.writeAll("void");
        if (mode == .zigonly) {
            try out.print("_{}", .{structure.typeNameID});
            return;
        }
        if (mode == .required) try out.print("blk: {{_ = ", .{});
        try structure.printType(out, .userdisplay);
        if (mode == .required) try out.print("; break :blk _{0};}}", .{structure.typeNameID});
    }
    fn printType(structure: Structure, out: anytype, mode: PrintMode) @TypeOf(out).Error!void {
        switch (structure.kind) {
            .unattached_magic => unreachable, // TODO report error unattached magic
            .struc => |sct| {
                try out.writeAll("struct {\n");
                for (sct.values) |val| {
                    if (val.field == null) continue;
                    try out.writeAll("    ");
                    try out.writeAll(val.field.?);
                    try out.writeAll(": ");
                    try val.structure.print(out, mode);
                    try out.writeAll(",\n");
                }
                try out.writeAll(
                    \\    _start: usize,
                    \\    _end: usize,
                );
                try out.writeAll("}");
            },
            .unio => |sct| {
                try out.writeAll("union(enum) {\n");
                for (sct.values) |val| {
                    try out.writeAll("    ");
                    try out.writeAll(val.field);
                    try out.writeAll(": ");
                    switch (val.magic) {
                        .none => |base| try base.print(out, mode),
                        .operator => try out.print("[]_{}", .{structure.typeNameID}),
                        .suffix => |cse| try out.print(
                            "struct {{ _: *_{}, {}: _{} }}",
                            .{ structure.typeNameID, cse.name.?, cse.typeNameID },
                        ),
                    }
                    try out.writeAll(",\n");
                }
                try out.writeAll("}");
            },
            .pointer => |ptr| {
                try out.writeAll("*");
                try writeTypeNameFor(out, ptr);
            },
            .value => |valu| {
                try writeTypeNameFor(out, valu);
            },
            .token => try out.writeAll("[]const u8"),
            .optional => |optv| {
                try out.writeAll("?");
                try optv.print(out, mode);
            },
            .array_only => |ao| {
                try out.writeAll("[]"); // []const ?
                try ao.item.print(out, mode);
            },
        }
    }
    fn printDecl(structure: Structure, out: anytype) @TypeOf(out).Error!void {
        try out.print("const _{} = ", .{structure.typeNameID});
        try structure.printType(out, .zigonly);
        try out.writeAll(";\n");
        switch (structure.kind) {
            .unattached_magic => unreachable, // TODO report error unattached magic
            .struc => |sct| {
                for (sct.values) |val| {
                    try val.structure.printDecl(out);
                }
            },
            .unio => |sct| {
                for (sct.values) |val| {
                    switch (val.magic) {
                        .none => |base| try base.printDecl(out),
                        .operator => |base| if (base) |b| try b.printDecl(out),
                        .suffix => |base| try base.printDecl(out),
                    }
                }
            },
            .pointer => {},
            .value => {},
            .token => {},
            .optional => |optv| {
                try optv.printDecl(out);
            },
            .array_only => |ao| {
                try ao.item.printDecl(out);
                if (ao.joiner) |jnr| try jnr.printDecl(out);
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
                            const uamName = std.meta.stringToEnum(enum { operator, suffix }, uam.name) orelse unreachable; // TODO report error :: invalid magic name
                            // uam.name, uam.args
                            switch (uamName) {
                                .operator => {
                                    // aaa the indentation is too much help pls
                                    const uamJoiner: ?Structure = if (uam.args.len == 1) blk: {
                                        const uamJoiner = try createForComponent(alloc, uam.args[0], gen);
                                        if (uamJoiner.name != null) unreachable; // TODO support named operators
                                        break :blk uamJoiner;
                                    } else if (uam.args.len == 0) blk: {
                                        break :blk null;
                                    } else unreachable; // TODO report error :: invalid number of args
                                    // ok so structure is actually = to []ThisUnion
                                    // which is weird
                                    // maybe it should have no structure then
                                    try resFields.append(.{
                                        .field = structure.name.?,
                                        .magic = .{ .operator = uamJoiner },
                                    });
                                },
                                .suffix => {
                                    if (uam.args.len != 1) unreachable; // TODO report error :: invalid number of args
                                    const uamSuffix = try createForComponent(alloc, uam.args[0], gen);
                                    if (uamSuffix.name == null) unreachable; // TODO support unnamed suffixes

                                    try resFields.append(.{
                                        .field = structure.name.?,
                                        .magic = .{ .suffix = uamSuffix },
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
            .decl_ref => |dr| return Structure.init(gen, dr, .{ .pointer = dr }),
            .parens => |pr| return createForComponent(alloc, pr.component.*, gen),
            .string => |str| {
                const expctdTkn = try parseString(alloc, str.*);
                return Structure.init(gen, null, .{ .token = .{ .kind = autoTokenKind(expctdTkn), .expected = expctdTkn } });
            },
            .token_ref => |token| {
                return Structure.init(gen, token.token, .{ .token = .{ .kind = token.token, .expected = null } });
            },
            .magic => |magic| return Structure.init(gen, null, .{
                .unattached_magic = .{ .name = magic.name, .args = magic.args },
            }),
            .suffixop => |sfxop| {
                const structure = try createForComponent(alloc, sfxop._.*, gen);
                switch (sfxop.suffixop.*) {
                    .nameset => |ns| return Structure.init(gen, ns.name, structure.kind),
                    .array => |ary| {
                        const allocatedStructure = switch (structure.kind) {
                            .pointer => |pname| try allocDupe(alloc, Structure.init(gen, structure.name, .{ .value = pname })),
                            else => try allocDupe(alloc, structure),
                        };
                        if (ary.component == null) {
                            return Structure.init(gen, null, .{ .array_only = .{ .item = allocatedStructure, .joiner = null } });
                        }

                        const joiner = try createForComponent(alloc, ary.component.?.*, gen);
                        if (joiner.name != null) unreachable; // TODO support named joiners
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

// miscompilation workaround
fn stringIf(condition: bool, one: []const u8, two: []const u8) []const u8 {
    if (condition) return one else return two;
}

fn autoTokenKind(token: []const u8) []const u8 {
    for (token) |char| switch (char) {
        'a'...'z', 'A'...'Z', '0'...'9', '_' => return "identifier",
        else => return "punctuation",
    };
    return "punctuation";
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
                        if (nost.*) |*nv| try nextCodegens.append(.{ .structure = nv, .fnid = joinerFnID });
                        const nextFunctionHalf = generator.nextID();

                        // imagine: automatically detect
                        // a = a '+' a | number
                        // that would be doable but bad idk
                        // anyway it would mean a would actually refer to the remainder of the union
                        // but with how this is currently programmed, that would be a mess

                        // oh my. is there a way to have named fields?
                        try out.print(
                            \\    var resAL = std.ArrayList(_{0}).init(parser.alloc);
                            \\    {4}_ = _{1}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => {{}}}}; // optional first joiner
                            \\    while (true) {{
                            \\        try resAL.append(_{2}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => {5}}});
                            \\        {4}_ = _{1}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break}};
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
                        , .{
                            structure.typeNameID,
                            joinerFnID,
                            nextFunctionHalf,
                            value.field,
                            stringIf(nost.* == null, "//", ""),
                            stringIf(nost.* == null, "break", "return parser.err(\"last or disallowed\")"),
                        });
                    },
                    .suffix => |*nost| {
                        const suffixopFnID = generator.nextID();
                        try nextCodegens.append(.{ .structure = nost, .fnid = suffixopFnID });
                        const nextFunctionHalf = generator.nextID();

                        if (false) {
                            const sb = parser.startBit();
                            errdefer parser.cancelBit(sb);

                            // 1: parseSingleComponent
                            const resc = parseComponent__NoSuffix(parser) catch return parser.err("no");
                            // 2: parse suffixop repeated
                            var resSuffixes = std.ArrayList(Suffixop).init(parser.arena);

                            while (true) {
                                try resSuffixes.append(parseSuffixop(parser) catch break);
                            }
                            // 3: unwrap
                            var topUnwrapped = resc;
                            for (resSuffixes.items) |suffix| {
                                const allocated = try parser.arena.create(Component);
                                allocated.* = topUnwrapped;
                                const allocatedSuffix = try parser.arena.create(Suffixop);
                                allocatedSuffix.* = suffix;
                                topUnwrapped = .{ .suffixop = .{ .component = allocated, .suffixop = allocatedSuffix } };
                            }
                            return topUnwrapped;
                        }

                        try out.print(
                            \\    // 1: parse the structure
                            \\    const resc = try _{0}(parser);
                            \\    // 2: parse the suffixop[] and unwrap
                            \\    var topUnwrapped = resc;
                            \\
                            \\    while (true) {{
                            \\        const suffix = _{1}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break}};
                            \\        const allocated = try parser.alloc.create(@TypeOf(topUnwrapped));
                            \\        allocated.* = topUnwrapped;
                            \\        topUnwrapped = .{{ .{2} = .{{ ._ = allocated, .{3} = suffix }} }};
                            \\    }}
                            \\    return topUnwrapped;
                            \\}}
                            \\fn _{0}(parser: *Parser) ParseError!_{4} {{
                            \\    const sb = parser.startBit();
                            \\    errdefer parser.cancelBit(sb);
                            \\
                        , .{ nextFunctionHalf, suffixopFnID, value.field, nost.name.?, structure.typeNameID });
                    },
                }
            }
            try out.writeAll("\n    return parser.err(\"union field not matched f\");");
        },
        .struc => |struc| {
            var resMap = std.ArrayList(struct { name: []const u8, id: usize }).init(alloc);
            try out.writeAll("    const start = parser.cpos;");
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

            try out.writeAll("    const end = parser.cpos;");
            try out.writeAll("    return ");
            try structure.print(out, .zigonly);
            try out.writeAll("{\n");
            for (resMap.items) |rv| {
                try out.print("        .{} = _{},\n", .{ rv.name, rv.id });
            }
            try out.writeAll("        ._start = start,");
            try out.writeAll("        ._end = end,");
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
        .token => |tk| {
            try out.print("    return (try _parseToken(parser, .{}, ", .{tk.kind});
            if (tk.expected) |xpcdt| try printZigString(xpcdt, out)
            //zig fmt
            else try out.writeAll("null");
            try out.writeAll(")).text;\n");
        },
        .array_only => |ao| {
            // item, joiner
            const itemFnid = generator.nextID();
            try nextCodegens.append(.{ .structure = ao.item, .fnid = itemFnid });
            const joinerFnid = generator.nextID();
            if (ao.joiner) |jnr| try nextCodegens.append(.{ .structure = jnr, .fnid = joinerFnid });
            @setEvalBranchQuota(1010100);
            try out.print(
                \\    var resAL = std.ArrayList(_{0}).init(parser.alloc);
                \\    while(true) {{
                \\        // :: parse 1 catch break
                \\        try resAL.append(_{1}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break}});
                \\        // :: parse 2 catch break
                \\        {3}_ = _{2}(parser) catch |e| switch(e) {{error.OutOfMemory => return e, error.ParseError => break}};
                \\    }}
                \\    return resAL.toOwnedSlice();
                \\
            , .{ ao.item.typeNameID, itemFnid, joinerFnid, stringIf(ao.joiner == null, "//", "") });
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
    }
}

pub const Generator = struct {
    alloc: *Alloc,
    uniqueid: usize = 0,

    pub fn nextID(gen: *Generator) usize {
        defer gen.uniqueid += 1;
        return gen.uniqueid;
    }

    pub fn parse(alloc: *Alloc, code: []const u8, out: anytype) !void {
        const file_decls = try parser.parse(alloc, code, .File);

        try out.writeAll(
            \\//! Autogenerated Code.
            \\//! Manual edits may be overwritten on rebuild.
        ++ "\n\n");

        var gen = Generator{ .alloc = alloc };

        var resStructures = std.ArrayList(Structure).init(alloc);
        defer resStructures.deinit();
        for (file_decls) |decl| {
            const structure = try Structure.createForComponent(alloc, decl.value.*, &gen);
            try resStructures.append(structure);

            try out.writeAll("/// ");
            try out.writeAll(decl.name);
            try out.writeAll(" = ");
            try printComponent(decl.value.*, out);
            try out.writeAll(";\n");

            // try out.writeAll("_ = ");
            // try structure.print(out, .userdisplay);
            // try out.writeAll(";\n");

            try out.writeAll("pub const ");
            try writeTypeNameFor(out, decl.name);
            try out.writeAll(" = ");
            try structure.print(out, .required);
            try out.writeAll(";\n");
        }

        try out.writeAll(
            \\//! Implementation below.
        ++ "\n\n");

        try out.writeAll(@embedFile("parser_header.zig"));
        // I would use ++ on embedFile but current zig makes that include the file twice, once without \n\n and once with it.
        try out.writeAll("\n\n");

        for (resStructures.items) |structure, i| {
            try structure.printDecl(out);

            const resid = gen.nextID();
            try codegenForStructure(alloc, &gen, structure, out, resid);
            try out.writeAll("pub const parse");
            try writeTypeNameFor(out, file_decls[i].name);
            try out.print(" = _{};\n", .{resid});
        }
    }

    pub fn deinit(generator: *Generator) void {}
};

pub fn main() !u8 {
    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const argv = try getArgs(alloc);
    if (argv.len < 2 or argv.len > 3) {
        std.debug.warn("usage: {} infile.resyn [outfile.zig]\n", .{argv[0]});
        return 1;
    }

    const code = try std.fs.cwd().readFileAlloc(alloc, argv[1], std.math.maxInt(usize));

    const filemode: enum { file, stdout } = if (argv.len > 2) .file else .stdout;
    const file = if (filemode == .file) try std.fs.cwd().createFile(argv[2], .{}) else std.io.getStdOut();
    defer if (filemode == .file) file.close() else {};

    const fileOut = file.writer();
    var bufferedOut = std.io.bufferedWriter(fileOut);

    const os = bufferedOut.writer();

    try Generator.parse(&arena.allocator, code, os);
    try bufferedOut.flush();

    return 0;
}

// makes an arraylist of args because it's easier to manage
fn getArgs(alloc: *Alloc) ![]const []const u8 {
    var args = std.process.ArgIterator.init();
    var res = std.ArrayList([]const u8).init(alloc);
    while (args.next(alloc)) |arg| try res.append(try arg);
    return res.toOwnedSlice();
}
