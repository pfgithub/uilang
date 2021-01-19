const ast = @import("uilang_parser");
const std = @import("std");
const Alloc = std.mem.Allocator;
const IR = @import("ir.zig").IR;
const Type = @import("type.zig").Type;
usingnamespace @import("../help.zig");

pub fn main() !void {
    const sample = @embedFile("sample.uil");

    const out = std.io.getStdOut().writer();

    try ast.printSyntaxHighlight(sample, out);

    try out.writeAll("\n\n------\n\n");

    var arena_alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_alloc.deinit();

    const alloc = &arena_alloc.allocator;

    const parsed = try ast.parse(alloc, sample, .File);

    try ast.printSyntaxHighlight(blk: {
        var al = std.ArrayList(u8).init(alloc);
        const al_out = al.writer();
        try printAst(parsed, al_out, .{});
        break :blk al.toOwnedSlice();
    }, out);

    const ns = try analyzeNamespace(alloc, parsed);

    // const declaration = ns.val.?.namespace.get("demoeg");

    // const fn_expr = declaration.analyze(alloc);

    // const ir_scope = IR_Env.new(alloc);

    // const resv = fn_expr.val.comptime_v.call(ir_scope, &[_]*Data{});

    // {
    //    ir_to_call_the_function();
    // }
}

const IR_Env = struct {
    instructions: std.ArrayList(IR_Line),
    pub fn new(alloc: *std.mem.Allocator) *IR_Env {
        return allocDupe(alloc, IR_Env{
            .instructions = std.ArrayList(IR_Line).init(alloc),
        }) catch @panic("oom");
    }
    pub fn add(env: *IR_Env, line: IR_Line) IR_Expr {
        const res = env.instructions.len;
        env.instructions.append(line) catch @panic("oom");
        return .{ .get_local = res };
    }
};
const IR_Line = union(enum) {
    fn_call: struct { fnc: IR_Expr, args: []IR_Expr },
};
const IR_Expr = union(enum) {
    get_local: usize,
};

fn analyzeNamespace(alloc: *Alloc, file: ast.File) !*Data {
    const root_env = Environment.newRoot(alloc);
    const root_ns = Namespace.new(alloc);

    for (file) |decl| {
        const name = decl.vardecl.vardecl.name.*;
        const initv = decl.vardecl.vardecl.initv;

        const decl_v = Declaration.new(alloc, root_env, initv);
        try root_ns.put(name, decl_v);
        try root_env.put(name, decl_v);
    }

    return Data.new(alloc, .ty, .{ .comptime_v = .{ .namespace = root_ns } });
}

fn analyzeExpression(alloc: *Alloc, env: *Environment, expr: ast.Expression) *Data {
    switch (expr) {
        .function => |fnxpr| {
            const fnv = Function.new(alloc, fnxpr);
            return Data.new(alloc, .function_ty, .{ .function = fnv });
        },
        else => std.debug.panic("TODO {}", .{std.meta.tagName(expr)}),
    }
}

const DataVal = union(enum) { comptime_v: Val, runtime_v: IR_Expr };
const Data = struct {
    pub fn new(alloc: *std.mem.Allocator, ty: Val, val: DataVal) *Data {
        return allocDupe(alloc, Data{ .ty = ty, .val = val }) catch @panic("oom");
    }
    ty: Val,
    val: DataVal, // null if the value is only known at runtime
};

// generic functions memoize different functions or comptime values based on the given argument types
const Function = struct {
    arg_types: []*Val,
    return_value: union(enum) { uninitialized, analyzing, initialized: *Data },

    pub fn new(alloc: *std.mem.Allocator, ast: ast.Function) *Function {
        // analyze argument types
        // if body is castexpresison<…>, analyze return type I guess
        if (ast.args.len > 0) @panic("TODO analyze argument types");

        return allocDupe(alloc, Function{
            .arg_types = &[_]*Val{},
            .return_value = .uninitialized,
        }) catch @panic();
    }
    pub fn call(fnc: *Function, alloc: *std.mem.Allocator, ir_env: *IR_Env, args: []*Data) *Data {
        if (args.len > 0) @panic("TODO function arguments");
        // :: cast the arguments
        // eg
        // const demo = fn(a: i32, b: i32) void: {}
        // demo(25, 50)
        // ::
        // %1 = 25
        // %2 = 50
        // %3 = @getconst(:fn:)
        // %4 = @as(i32, %1)
        // %5 = @as(i32, %2)
        // %6 = @call(%3, %4, %5)
        // or something idk
        if (fnc.return_value == .analyzing) @panic("analysis loop");
        const res_type = fnc.analyze();
        // add a thing to the ir @call(res_type.val) → res_type.ty;
        // uuh how
    }
};

// const MemoizedType = struct {
//     args: []*Data,
//     return_value: *Data,
//     // this is a comptime value :: a function that is not generic
//     // tbh there's no reason to make generic functions yet if they're not
// };
// const Function = struct {
//     memoized_types: std.ArrayList(MemoizedType),
//     args: []FunctionArg,
//     alloc: *std.mem.Allocator,
//     const FunctionArg = struct {};

//     pub fn new(alloc: *std.mem.Allocator) *Function {
//         return allocDupe(alloc, Function{
//             .memoized_types = std.ArrayList(MemoizedType).init(alloc),
//             .args = &[_]FunctionArg{},
//             .alloc = alloc,
//         }) catch @panic("oom");
//     }

//     fn codegen(function: *Function, final_arg_types: []*Data) MemoizedType {
//         if (final_arg_types.len > 0) @panic("TODO");
//         if (function.args.len > 0) @panic("TODO");

//         if (function.memoized_types.len > 0) {
//             return function.memoized_types[0];
//         }
//         const at_dupe = function.alloc.dupe(*Data, final_arg_types);
//         function.memoized_types.append(MemoizedType{args: final_arg_types, data: });
//     }

//     pub fn call(function: *Function, ir_env: *IR_Env, args: []*Data) *Data {
//         if (function.args.len > 0) @panic("TODO");
//         const codegen_res = function.codegen(&[_]*Data{});
//         // call the codegen'd function
//     }
// };

const Val = union(enum) {
    ty,
    function_ty,
    number: f64,
    string: []const u8,
    namespace: *Namespace,
    function: *Function,
};

const Declaration = struct {
    value: union(enum) {
        uninitialized: struct {
            env: *Environment,
            code: *ast.Expression,
        },
        analyzing: void,
        initialized: struct {
            data: *Data,
        },
    },
    pub fn analyze(decl: *Declaration, alloc: *std.mem.Allocator) *Data {
        switch (decl.value) {
            .uninitialized => |unin| {
                const env = unin.env;
                const code = unin.code;
                unin.value = .analyzing;
                const resv = analyzeExpression(alloc, env, code);
                unin.value = .{ .initialized = .{ .data = resv } };
                return resv;
            },
            .analyzing => @panic("analysis loop"),
            .initialized => |v| return v.data,
        }
    }
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
    pub fn get(ns: *Namespace, name: []const u8) ?*Declaration {
        return ns.declarations.get(name);
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
                .pipeline => |pplne| {
                    try out.writeAll("^"); // »
                    try printAst(pplne.expression.*, out, indent);
                },
                else => try out.print("[@todo.{s}]", .{std.meta.tagName(sfxop.suffixop.*)}),
            }
        },
        else => try out.print("@todo(.{s})", .{std.meta.tagName(node)}),
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
            try out.print("{s} {s} = ", .{
                switch (node.vartype) {
                    .const_ => @as([]const u8, "const"),
                    .let => "var",
                    .state => "state",
                    .memo => "memo",
                    .trigger => "trigger",
                },
                node.name.*,
            });
            try printAst(node.initv.*, out, indent);
        },
        ast.Expression => try printExpr(node, out, indent),
        ast.Function => {
            try out.print("{s}(", .{switch (node.kind) {
                .function => @as([]const u8, "fn"),
                .widget => "widget",
            }});
            for (node.args) |arg, i| {
                if (i != 0) try out.writeAll(", ");
                try out.print("{s}", .{arg});
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
            try out.print("@{s}(", .{node.name.*});
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
