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

    try ast.printSyntaxHighlight(blk: {
        var al = std.ArrayList(u8).init(alloc);
        const al_out = al.writer();
        try printAst(parsed, al_out, .{});
        break :blk al.toOwnedSlice();
    }, out);

    const root_env = Environment.newRoot(alloc);
    const root_ns = Namespace.new(alloc);

    for (parsed) |decl| {
        const name = decl.vardecl.vardecl.name.*;
        const initv = decl.vardecl.vardecl.initv;

        const decl_v = Declaration.new(alloc, root_env, initv);
        try root_ns.put(name, decl_v);
        try root_env.put(name, decl_v);
    }

    //root_ns.get("main");

    // parse file into root_ns and root_env
}

const Data = struct {
    ty: *Val,
    val: ?*Val, // null if the value is only known at runtime
};

// const a = "hi";
// typeof a is "hi"
// const a = string: "hi"
// typeof a is type{string}
// const b = "hi": a;
// this is not valid; a is string, string might ≠ "hi"
// ah ok

const Val = union(enum) {
    typ: struct {
        // workaround because zig compiler doesn't like `type: @TagType(@This())` atm
        value: u32,
        fn get(slf: @This()) @TagType(Val) {
            return @intToEnum(@TagType(Val), slf.value);
        }
        fn set(val: @TagType(Val)) @This() {
            return .{ .value = @enumToInt(val) };
        }
    },
    number: f64,
    float: f64,
    comptime_string: []const StringBit,
    string: []const u8,
};

const Declaration = struct {
    value: union(enum) {
        uninitialized: struct {
            env: *Environment,
            code: *ast.Expression,
        },
        analyzing: void,
        initialized: struct {
            data: Data,
        },
    },
    pub fn new(alloc: *std.mem.Allocator, env: *Environment, code: *ast.Expression) *Declaration {
        return allocDupe(alloc, Declaration{
            .value = .{ .uninitialized = .{ .env = env, .code = code } },
        }) catch @panic("oom");
    }
};

const StringBit = union(enum) {
    text: []const u8,
    sub_bit: *StringBit, // "\{one\{two}three}", to be used for eg std.fmt("Hi \{name}", .name="test");
};

const Namespace = struct {
    declarations: std.StringHashMap(*Declaration),

    pub fn new(alloc: *std.mem.Allocator) *Namespace {
        return allocDupe(alloc, Namespace{
            .declarations = std.StringHashMap(*Declaration).init(alloc),
        }) catch @panic("oom");
    }
    pub fn put(ns: *Namespace, name: []const u8, decl: *Declaration) !void {
        const v = ns.declarations.getOrPut(name) catch @panic("oom");
        if (v.found_existing) return error.DuplicateVarName;
        v.entry.value = decl;
    }
};
const Environment = struct {
    alloc: *std.mem.Allocator,
    parent: ?*Environment,
    declarations: std.StringHashMap(*Declaration),

    pub fn newRoot(alloc: *std.mem.Allocator) *Environment {
        return allocDupe(alloc, Environment{
            .alloc = alloc,
            .parent = null,
            .declarations = std.StringHashMap(*Declaration).init(alloc),
        }) catch @panic("oom");
    }

    pub fn new(parent_env: *Environment) *Environment {
        return allocDupe(parent_env.alloc, Environment{
            .alloc = parent_env.alloc,
            .parent = parent_env,
            .declarations = std.StringHashMap(*Declaration).init(alloc),
        }) catch @panic("oom");
    }

    pub fn put(env: *Environment, name: []const u8, decl: *Declaration) !void {
        // todo no shadow
        const v = env.declarations.getOrPut(name) catch @panic("oom");
        if (v.found_existing) return error.DuplicateVarName;
        v.entry.value = decl;
    }
};

fn addDecl(
    env: *Environment,
    namespace: *Namespace,
) void {}

fn readNamespace(node: ast.File, parent_env: *Environment) Namespace {
    var env = Environment.new(parent_env);
    var namespace = Namespace.new(parent_env.alloc);

    for (node) |decl| {
        addDecl(&env, &namespace, decl);
    }
}

fn printExpr(node: ast.Expression, out: anytype, indent: IndentWriter) @TypeOf(out).Error!void {
    switch (node) {
        .function => |n| try printAst(n.*, out, indent),
        .builtinexpr => |n| try printAst(n.*, out, indent),
        .string => |n| try printAst(n.*, out, indent),
        .block => |n| try printAst(n.*, out, indent),
        .variable => |n| try printAst(n.*, out, indent),
        .vardecl => |n| try printAst(n.*, out, indent),
        .suffixop => |sfxop| {
            try printAst(sfxop._.*, out, indent);
            switch (sfxop.suffixop.*) {
                .implicitcast => |implc| {
                    try out.writeAll(": ");
                    try printAst(implc.expression.*, out, indent);
                },
                else => try out.print("«<{}>»", .{std.meta.tagName(sfxop.suffixop.*)}),
            }
        },
        else => try out.print("@todo(.{})", .{std.meta.tagName(node)}),
    }
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
            .vardecl => |vd| {
                if (vd.public) |_| try out.writeAll("pub ");
                try printAst(vd.vardecl.*, out, indent);
            },
            else => @panic("notprintsupport"),
        },
        ast.Vardecl => {
            try out.print("{} {} = ", .{ std.meta.tagName(node.vartype), node.name.* });
            try printAst(node.initv.*, out, indent);
        },
        ast.Expression => try printExpr(node, out, indent),
        ast.Function => {
            try out.print("{}(", .{std.meta.tagName(node.kind)});
            for (node.args) |arg, i| {
                if (i != 0) try out.writeAll(", ");
                try out.print("{}", .{arg});
            }
            try out.writeAll(") ");
            try printAst(node.expression.*, out, indent);
        },
        ast.String => {
            try out.writeByte('"');
            for (node.bits) |bit| switch (bit) {
                .string => |sbit| try out.writeAll(sbit),
                .escape => |escbit| try out.writeAll(escbit),
            };
            try out.writeByte('"');
        },
        ast.Builtinexpr => {
            // name: *Identifier, args: []expression
            try out.print("@{}(", .{node.name.*});
            for (node.args) |arg, i| {
                if (i != 0) try out.writeAll(", ");
                try printAst(arg, out, indent);
            }
            try out.writeAll(")");
        },
        ast.Block => {
            try out.writeAll("{");
            for (node.decls) |expr, i| {
                try out.print("\n{}", .{indent.add(1)});
                try printAst(expr, out, indent.add(1));
                try out.writeAll(";");
            }
            if (node.decls.len > 0) try out.print("\n{}", .{indent});
            try out.writeAll("}");
        },
        ast.Variable => {
            try out.writeAll(node.name.*);
        },

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
