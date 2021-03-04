const ast = @import("uilang_parser");
const std = @import("std");
const Alloc = std.mem.Allocator;
usingnamespace @import("../../help.zig");

pub fn main() !void {
    var arena_alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_alloc.deinit();
    const alloc = &arena_alloc.allocator;

    const sample = @embedFile("./demo.uil");

    const out = std.io.getStdOut().writer();

    try ast.printSyntaxHighlight(sample, out);

    try out.writeAll("\n\n------\n\n");

    const parsed: []ast.Expression = try ast.parse(alloc, sample, .File);

    // init a scope
    // generate ir for a code block

    const ir_block = IR.newBlock(alloc);
    const scope = Scope.new(alloc, null, ir_block, .none);

    // part 1: find and define all variables
    // part 2: go through and evaluate
    for (parsed) |*expr| {
        try preinitVariables(scope, expr);
    }
    for (parsed) |*expr| {
        _ = try evaluateExpression(scope, expr);
    }
}

pub fn preinitVariables(scope: *Scope, expr: *ast.Expression) RuntimeError!void {
    switch (expr.*) {
        .vardecl => |vd| {
            switch (vd.vartype) {
                .const_ => {},
                else => {
                    // todo displayErrorAt(expr.vartype)
                    return displayErrorAt(expr, "TODO unsupported var type {std.meta.tagName(vd.vartype)}");
                },
            }
            scope.makeVariable(vd.name.*, vd.initv) catch |e| switch (e) {
                error.AlreadyDefined => return displayErrorAt(expr, "variable is already defined"),
            };
        },
        else => {},
    }
}

// these expressions create a new scope:
// () eg (let a = int: 25, a)
// {} eg <blkname>{let a = int: 25; return<blkname> 25;}
// <…> eg <blkname>({return <blkname> 25;}) // uuh maybe <blkname> should just be a thing that goes before parens or curlies
pub fn evaluateExpression(scope: *Scope, expr: *ast.Expression) RuntimeError!*IR {
    switch (expr.*) {
        .vardecl => {
            // get the varinfo for this variable
            // emit const safe(varname) = evaluateExpression(value)
            return allocDupe(scope.name_map.allocator, IR{ .todo = .{} }) catch @panic("oom");
        },
        else => {
            std.debug.panic("TODO expression {s}", .{std.meta.tagName(expr.*)});
        },
    }
}

pub const IR = union(enum) {
    todo,
    block: struct {
        code: std.ArrayList(*IR),
    },
    pub fn newBlock(alloc: *std.mem.Allocator) *IR {
        return allocDupe(alloc, IR{ .block = .{ .code = std.ArrayList(*IR).init(alloc) } }) catch @panic("oom");
    }
};

const Type = struct {
    value: union(enum) {
        exact_number, // eg 25
        int, // eg int: 25
        function: struct { // eg fn() void: {}
            args: []*Type,
            ret_v: *Type,
        },
        never, // noreturn / ⊥bottom type
    },
};

// (int: 25) → int
// some_var → typeof(somevar)
// 25 → 25
// 5 + 6 → 11
// (const recurse = fn() void: {return recurse()}, recurse) → fn(): void
// (const a = return 3) → never (⊥)
pub fn typeOfExpression(scope: *Scope, expr: *ast.Expression) RuntimeError!*Type {
    // if expression is type":"expression return type
    // else @panic("todo infer types")
    switch (expr.*) {
        // type: value => evaluateTypeExpression(type)
        else => {
            std.debug.panic("TODO infer type of expression .{s}", .{std.meta.tagName(expr.*)});
        },
    }
}

// first, x is added with preinitVariables
// then, when print(x) is encountered, x is type analyzed to get its type (25)
// type-analysis does the minimum possible to get the type. `int: …` can exit immediately because it knows the type is "int"
// when var x is encountered, it is already analyzed and ir just has to be inserted

// const y = fn() {print(x)}
// var x = 25;
pub const VariableInitializer = struct {
    value: union(enum) {
        uninitialized: struct {
            expr: *ast.Expression,
            scope: *Scope,
        },
        progress,
        ready: struct {
            ty: *Type,
        },
    },
    pub fn getType(vi: *VariableInitializer) RuntimeError!*Type {
        return switch (vi.value) {
            .uninitialized => |un_temp| {
                const un = un_temp;
                vi.value = .progress;
                const type_v = try typeOfExpression(un.scope, un.expr);
                vi.value = .{ .ready = .{ .ty = type_v } };
                return type_v;
            },
            // eg `const a = b; const b = a;` that will cause this error because the types cannot be resolved
            // `const a = int: b; const b = int: a` is fine though and will just cause a runtime error / possibly a different compile time error.
            .progress => @panic("TODO Error: Loop found"),

            .ready => |rdy| rdy.ty,
        };
    }
};

const RuntimeError = error{ErrorAt};

pub fn displayErrorAt(expression: *ast.Expression, msg: []const u8) error{ErrorAt} {
    std.log.err("error line x col x: {s}", .{msg});
    return error.ErrorAt;
}

// scopes are created:
// when any block opens {}
// when any parens open () I guess
// when any scope label is used
pub const Scope = struct {
    parent: ?*Scope,
    cf_catch: CFCatch,
    ir_block: *IR,
    name_map: NameMap,
    const CFCatch = union(enum) {
        ret,
        label: []const u8,
        none,
    };
    const NameMap = std.StringHashMap(*VariableInitializer);
    pub fn new(alloc: *std.mem.Allocator, parent: ?*Scope, ir_block: *IR, cf_catch: CFCatch) *Scope {
        var allocated = alloc.create(Scope) catch @panic("oom");

        allocated.* = .{
            .parent = parent,
            .cf_catch = cf_catch,
            .ir_block = ir_block,
            .name_map = NameMap.init(alloc),
        };

        return allocated;
    }

    pub fn makeVariable(scope: *Scope, name: []const u8, expr: *ast.Expression) error{AlreadyDefined}!void {
        // TODO if already defined, error
        const vinitializer = allocDupe(
            scope.name_map.allocator,
            VariableInitializer{ .value = .{ .uninitialized = .{ .expr = expr, .scope = scope } } },
        ) catch @panic("oom");
        const prev_v = scope.name_map.fetchPut(name, vinitializer) catch @panic("oom");
        if (prev_v) |pv| {
            return error.AlreadyDefined;
        }
    }
};

// <break>: while(true) <continue>: {
//     return<continue> 25;
//     return<break> 10;
// }

// <label>:
