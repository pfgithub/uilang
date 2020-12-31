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

const StructMagic = union(enum) {
    none: Structure,
    lockin: void,
};
const StructField = struct { field: ?[]const u8, magic: StructMagic };
const UnionMagic = union(enum) {
    none: Structure,
    operator: ?*Structure,
    suffix: Structure,
};
const UnionField = struct { field: []const u8, magic: UnionMagic };

const MagicType = union(enum) {
    operator: ?*Structure,
    suffix: *Structure,
    lockin: void,
};

const StructureKind = union(enum) {
    none,
    unattached_magic: MagicType,
    unio: struct {
        values: []UnionField,
    },
    struc: struct {
        values: []StructField,
    },
    pointer: []const u8,
    value: []const u8,
    token: struct { kind: []const u8, expected: ?[]const u8 },
    optional: *Structure,
    array_only: struct {
        item: *Structure,
        joiner: ?*Structure,
    },
};
const PrintMode = enum { required, userdisplay, zigonly };
// zig fmt: off
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
            .none => try out.writeAll("void"), // TODO u0 when stage2 is the default zig compiler
            .unattached_magic => unreachable, // TODO report error unattached magic
            .struc => |sct| {
                try out.writeAll("struct {\n");
                for (sct.values) |val| {
                    switch (val.magic) {
                        .none => |nmgi| {
                            if (val.field == null) {
                                try out.writeAll("// <unnamed>: …\n");
                                continue;
                            }
                            try out.writeAll("    ");
                            try out.writeAll(val.field.?);
                            try out.writeAll(": ");
                            try nmgi.print(out, mode);
                            try out.writeAll(",\n");
                        },
                        .lockin => {
                            try out.writeAll("// <lockin past this point>\n");
                        },
                    }
                }
                try out.writeAll(
                    \\
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
                        .operator => |op| {
                            try out.writeAll("[]");
                            if (op) |opv| if (opv.name) |opname| {
                                try out.writeAll("union(enum) {");
                                try out.writeAll(opname);
                                try out.writeAll(": ");
                                try opv.print(out, mode);
                                try out.writeAll(", _: ");
                            };
                            try out.print("_{}", .{structure.typeNameID});
                            if (op) |opv|
                                if (opv.name) |opname| {
                                    try out.writeAll("}");
                                };
                        },
                        .suffix => |cse| {
                            try out.print(
                                "struct {{ _: *_{}, {}: ",
                                .{ structure.typeNameID, cse.name.? },
                            );
                            try cse.print(out, mode);
                            try out.writeAll("}");
                        },
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
            .token => |tk| if (tk.expected == null) try out.writeAll("[]const u8") else try out.writeAll("Token"),
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
                for (sct.values) |val| switch (val.magic) {
                    .none => |substructure| try substructure.printDecl(out),
                    .lockin => {},
                };
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
            .none => {},
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
            .force_struct => return Structure.init(gen, null, .none),
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
                        .unattached_magic => |uam| switch (uam) {
                            .operator => |operator| try resFields.append(.{
                                .field = structure.name.?,
                                .magic = .{ .operator = operator },
                            }),
                            .suffix => |suffix| {
                                if (suffix.name == null) unreachable; // TODO support unnamed suffixes

                                try resFields.append(.{
                                    .field = structure.name.?,
                                    .magic = .{ .suffix = suffix.* },
                                });
                            },
                            else => unreachable, // TODO report error :: invalid magic
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

                    switch (structure.kind) {
                        .unattached_magic => |uam| switch (uam) {
                            .lockin => try resFields.append(.{ .field = structure.name, .magic = .lockin }),
                            else => unreachable, // TODO error can't use that type of uam here
                        },
                        .none => {},
                        else => try resFields.append(
                            .{ .field = structure.name, .magic = .{ .none = structure } },
                        ),
                    }
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
            .magic => |magic| {
                const magicType = std.meta.stringToEnum(@TagType(MagicType), magic.name) orelse unreachable; // TODO report error :: bad magic
                switch (magicType) {
                    .suffix => {
                        if (magic.args.len != 1) unreachable; // TODO report error :: missing args
                        const substructure = try createForComponent(alloc, magic.args[0], gen);
                        return Structure.init(gen, substructure.name, .{ .unattached_magic = .{ .suffix = try allocDupe(alloc, substructure) } });
                    },
                    .operator => {
                        if (magic.args.len == 0) return Structure.init(gen, null, .{ .unattached_magic = .{ .operator = null } });
                        if (magic.args.len > 1) unreachable; // TODO report error :: bad args count
                        const substructure = try createForComponent(alloc, magic.args[0], gen);
                        return Structure.init(gen, null, .{ .unattached_magic = .{ .operator = try allocDupe(alloc, substructure) } });
                    },
                    .lockin => {
                        if (magic.args.len == 1) unreachable; // TODO support #lockin("error message")
                        if (magic.args.len > 1) unreachable; // TODO report error :: bad args count
                        return Structure.init(gen, null, .{ .unattached_magic = .lockin });
                    },
                }
            },
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
// zig fmt: on

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
                            \\        return _{}{{ .{} = _{}(parser) catch |e| switch(e) {{else => return e, error.Recoverable => break :blk}} }};
                            \\    }}
                        ,
                            .{ structure.typeNameID, value.field, fnid },
                        );
                    },
                    .operator => |*nost| {
                        const joinerFnID = generator.nextID();
                        if (nost.*) |nv| try nextCodegens.append(.{ .structure = nv, .fnid = joinerFnID });
                        const nextFunctionHalf = generator.nextID();

                        // imagine: automatically detect
                        // a = a '+' a | number
                        // that would be doable but bad idk
                        // anyway it would mean a would actually refer to the remainder of the union
                        // but with how this is currently programmed, that would be a mess

                        // oh my. is there a way to have named fields?

                        if (nost.*) |nst| {
                            if (nst.name) |_| {}
                        }
                        const named: ?[]const u8 = if (nost.*) |nst| if (nst.name) |nme| nme else null else null;
                        const named_compiler_hack_2: []const u8 = "_";
                        const named_compiler_hack = named orelse named_compiler_hack_2;

                        try out.print(
                            \\    const FieldType = std.meta.Child(std.meta.fieldInfo(_{0}, "{3}").field_type);
                            \\    var resAL = std.ArrayList(FieldType).init(parser.alloc);
                            \\    {9}{4}_ = _{1}(parser) catch |e| switch(e) {{else => return e, error.Recoverable => {{}}}}; // optional first joiner
                            \\    // optional first joiner is not allowed with a named arg. that's a dumb special case but whatever
                            \\    while (true) {{
                            \\        const slot = try resAL.addOne();
                            \\        {{errdefer _ = resAL.pop(); slot.* = {6}(_{2}(parser) catch |e| switch(e) {{else => return e, error.Recoverable => {5}}}) {7};}}
                            \\        {8}const opslot = try resAL.addOne();
                            \\        {{errdefer _ = resAL.pop(); {4}{12} = {11}{{ .{10} = _{1}(parser) catch |e| switch(e) {{
                            \\        {4}    else => return e, error.Recoverable => {{_ = resAL.pop(); break;}}
                            \\        {4}}}}};
                            \\        }}
                            \\    }}
                            \\    if (resAL.items.len == 0) return parser.err("no items");
                            \\    if (resAL.items.len == 1) return resAL.items[0]{13};
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
                            stringIf(named != null, ".{._ = ", ""),
                            stringIf(named != null, "}", ""),
                            stringIf(named == null, "//", ""),
                            stringIf(named == null, "", "//"),
                            named_compiler_hack,
                            stringIf(named == null, "struct{_: FieldType}", "."),
                            stringIf(named == null, "_", "opslot.*"),
                            stringIf(named == null, "", "._"),
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
                            \\        const suffix = _{1}(parser) catch |e| switch(e) {{else => return e, error.Recoverable => break}};
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
                switch (value.magic) {
                    .none => |*substructure| {
                        const fnid = generator.nextID();
                        try nextCodegens.append(.{ .structure = substructure, .fnid = fnid });

                        if (value.field) |nme| {
                            const id = generator.nextID();
                            try out.print("    const _{} = ", .{id});
                            try resMap.append(.{ .name = nme, .id = id });
                        } else try out.print("    _ = ", .{});
                        try out.print("try _{}(parser);\n", .{fnid});
                    },
                    .lockin => {
                        const nextFunctionHalf = generator.nextID();
                        const randomid = generator.nextID();
                        // also we need to loop over resmap and pass those too
                        // this is where catch on blocks would be useful
                        // (because errdefer can't return something) (that would be a solution too)
                        // {
                        //     errdefer return reportError();
                        // }
                        try out.print(
                            \\    return _{0}(parser, start
                        , .{
                            nextFunctionHalf,
                        });
                        for (resMap.items) |rv| {
                            try out.print(", _{}", .{rv.id});
                        }
                        try out.writeAll(") catch |e| switch(e) {error.Recoverable => ");
                        try out.print("return parser.unrecoverableError(\"lockin failed ({})\")", .{randomid});
                        try out.writeAll(", else => return e};");
                        try out.writeAll("}");
                        try out.print("fn _{0}(parser: *Parser, start: usize", .{nextFunctionHalf});
                        for (resMap.items) |rv| {
                            try out.print(", _{}: anytype", .{rv.id});
                        }
                        try out.print(") ParseError!_{} {{\n", .{structure.typeNameID});
                        try out.writeAll(
                            \\    const sb = parser.startBit();
                            \\    errdefer parser.cancelBit(sb);
                        );

                        // ) catch @panic("locked in item did not pass"); // TODO error reporting
                        // \\}}
                        // \\fn _{0}(parser: *Parser) ParseError!_{structure.typeNameID} {{
                        // :: return parse second half catch panic
                        // fn _{0}
                    },
                }
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
                \\        else => return e,
                \\        error.Recoverable => return null, // note that the called function already cancelBit'd so it's ok
                \\    };
                \\
            );
        },
        .token => |tk| {
            try out.writeAll("    return ");
            try out.print("    (try _parseToken(parser, .{}, ", .{tk.kind});
            if (tk.expected) |xpcdt| try printZigString(xpcdt, out) //zig fmt
            else try out.writeAll("null");
            if (tk.expected == null) try out.writeAll(")).text;\n") //zig fmt
            else try out.writeAll("));\n");
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
                \\        try resAL.append(_{1}(parser) catch |e| switch(e) {{else => return e, error.Recoverable => break}});
                \\        // :: parse 2 catch break
                \\        {3}_ = _{2}(parser) catch |e| switch(e) {{else => return e, error.Recoverable => break}};
                \\    }}
                \\    return resAL.toOwnedSlice();
                \\
            , .{ ao.item.typeNameID, itemFnid, joinerFnid, stringIf(ao.joiner == null, "//", "") });
        },
        .none => try out.writeAll("    return {};"),
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
            for (stri.bits) |bit| switch (bit) {
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
            try structure.print(out, .zigonly);
            try out.writeAll(";\n");

            try out.writeAll("const _");
            try writeTypeNameFor(out, decl.name);
            try out.writeAll(" = ");
            try structure.print(out, .userdisplay);
            try out.writeAll(";\n\n");
        }

        try out.writeAll(
            \\
            \\//.
            \\//.
            \\//.
            \\//.
            \\//.
            \\//.
            \\//.
            \\//                 / ...=====================... \
            \\// --------=======<     Implementation below.     >=======--------
            \\//                 \ '''=====================''' /
            \\//.
            \\//.
            \\//.
            \\//.
            \\//.
            \\//.
            \\//.
            \\
            ++ "\n");

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
