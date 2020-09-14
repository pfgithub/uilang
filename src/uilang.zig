const std = @import("std");
const ast = @import("uilang_parser");
const Alloc = std.mem.Allocator;

const ID = u64;

// in case this is ever made multithreaded, each thread will need a new starting point for this id (c pthread_self() * like std.math.maxInt(u50))
threadlocal var id: ID = 0;

fn getNewID() usize {
    defer id += 1;
    return id;
}

// prefix is used eg some variables want a prefix
// in the future we will have sourcemaps and this won't matter as much
fn createJSSafeName(alloc: *Alloc, name: []const u8, prefix: []const u8) ![]const u8 {
    var res = std.ArrayList(u8).init(alloc);
    try res.appendSlice(prefix);

    for (name) |char| {
        switch (char) {
            'a'...'z', 'A'...'Z', '_' => try res.append(char),
            '0'...'9' => {
                if (res.items.len == 0) try res.appendSlice("_");
                try res.append(char);
            },
            else => {
                try res.appendSlice("ˀ");
            },
        }
    }

    const uniqud = getNewID();
    try res.writer().print("_{x}", .{uniqud});

    return res.toOwnedSlice();
}

fn allocDupe(alloc: *Alloc, a: anytype) !*@TypeOf(a) {
    const c = try alloc.create(@TypeOf(a));
    c.* = a;
    return c;
}

const LaterType = union(enum) {
    uninitialized_type: struct {
        environment: *Environment,
        type_code: *ast.Expression,
    },
    // in this case, the expression needs to be evaluated and then latertype will actually
    // be storing the result ir to be inserted with the vardecl
    uninitialized_expression: struct {
        environment: *Environment,
        expression_code: *ast.Expression,
    },
    in_progress: void,
    initialized: struct {
        t_type: Type,
        ir: ?*IR,
    },
    fn initialize(lt: *LaterType) EvalExprError!void {
        if (lt.* == .initialized) return;
        const copy = lt.*;
        lt.* = .in_progress;
        switch (copy) {
            .uninitialized_type => |unin| {
                const env = unin.environment;
                const type_code = unin.type_code;
                const exprval = try evaluateExpr(env, type_code.*);
                if (exprval.t_type != .t_type) {
                    // uh oh
                    return reportError("Expected type, got {}");
                }
                // comptimeEval(exprval.ir) // what does this do? idk, something.
                if (exprval.ir != .t_type) {
                    @panic("TODO comptimeEval(exprval.ir)");
                }
                lt.* = .{
                    .initialized = .{ .ir = null, .t_type = exprval.ir.t_type },
                };
            },
            .uninitialized_expression => |uninexpr| {
                const env = uninexpr.environment;
                const exprcode = uninexpr.expression_code;
                const exprval = try evaluateExpr(env, exprcode.*);
                const allocdir = try allocDupe(env.alloc, exprval.ir);
                lt.* = .{
                    .initialized = .{ .ir = allocdir, .t_type = exprval.t_type },
                };
            },
            .initialized => unreachable, // handled above
            .in_progress => return reportError("Circular dependency"),
        }
    }
};

const DeclInfo = struct {
    const DeclKind = enum { const_, let, state, trigger };
    kind: DeclKind,
    decl_type: LaterType,
    jsname: []const u8,
    fn init(alloc: *Alloc, kind: DeclKind, name: []const u8, env: *Environment, typev: ?*ast.Expression, valuev: *ast.Expression) !DeclInfo {
        return DeclInfo{
            .kind = kind,
            .decl_type = if (typev) |tv|
                LaterType{ .uninitialized_type = .{ .environment = env, .type_code = tv } }
            else
                LaterType{ .uninitialized_expression = .{ .environment = env, .expression_code = valuev } },
            .jsname = try createJSSafeName(alloc, name, switch (kind) {
                // TODO if this miscompiles, move it into a new function and have it return.
                .state => "$",
                .trigger => "ꓕ",
                .const_ => "",
                .let => "",
            }),
        };
    }
};

// gives information about variables and decls and stuff
const Environment = struct {
    alloc: *Alloc,
    parent: ?*Environment,
    catchReturns: bool, // if this environment layer catches return statements
    // deferStatements: unmanagedArrayList(something), for if we ever do defer statements
    _variables: std.StringHashMap(DeclInfo),
    const EnvironmentConfig = struct {
        catchReturns: bool,
    };
    fn init(alloc: *Alloc, parent: ?*Environment, config: EnvironmentConfig) Environment {
        return Environment{
            .alloc = alloc,
            .parent = parent,
            .catchReturns = config.catchReturns,
            ._variables = std.StringHashMap(DeclInfo).init(alloc),
        };
    }
    // I think we might need gc. no deinit for now then.
    // idk actually. gc is only necessary if values get used after their decl returned
    // which doesn't happen I think
    fn deinit(me: *Environment) void {
        me._variables.deinit();
        me.* = undefined;
    }

    fn declare(env: *Environment, name: []const u8, kind: DeclInfo.DeclKind, typev: ?*ast.Expression, valuev: *ast.Expression) !void {
        const slot = try env._variables.getOrPut(name);
        if (slot.found_existing) return reportError("Variable already declared in this scope"); // todo decide whether to allow shadowing or not
        slot.entry.value = try DeclInfo.init(env.alloc, kind, name, env, typev, valuev);
    }
    fn get(env: *Environment, name: []const u8) ?*DeclInfo {
        const entry = env._variables.getEntry(name) orelse {
            if (env.parent) |parent| return parent.get(name);
            return null;
        };
        return &entry.value;
    }
};

fn evalDeclArray(decls: []ast.Expression, env: *Environment) void {}

const ReportedError = error{Reported};
fn reportError(msg: []const u8) ReportedError {
    std.log.err("Error: {}", .{msg});
    return ReportedError.Reported;
}

const Type = union(enum) {
    unit, // the value cannot be used, there is no value.
    t_type, // represents a type. only available at compiletime.
};
const ExprEvalResult = struct {
    t_type: Type,
    ir: IR,
};

const EvalExprError = ReportedError || error{OutOfMemory};
// if we do result location stuff, we can skip a bit
// resultLocation: struct {result: Type, outname: []const u8}
// -> struct {result: Type}
fn evaluateExprs(environment: *Environment, decls: []ast.Expression) EvalExprError!ExprEvalResult {
    // 1: hoist vardecls
    for (decls) |decl| switch (decl) {
        .vardecl => |vd| {
            // add decl to environment
            const vt: DeclInfo.DeclKind = switch (vd.vartype) {
                .let => .let,
                .const_ => .const_,
                .state => .state,
                .trigger => .trigger,
            };
            try environment.declare(vd.name.*, vt, if (vd.ttype) |tt| tt.expression else null, vd.expression);
        },
        else => {}, // ok
    };

    var resIR = std.ArrayList(IR).init(environment.alloc);

    // 2: evaluate expressions
    for (decls) |decl| {
        const resultValue = try evaluateExpr(environment, decl);
        if (resultValue.t_type != .unit) return reportError("Expected void return value");
        try resIR.append(resultValue.ir);
    }

    // if any of those decls return, the info will be written to environment
    // this can be used to determine the return type
    return ExprEvalResult{
        .t_type = .unit,
        .ir = IR{ .block = resIR.toOwnedSlice() },
    };
}

/// store ir value into to_type, coercing if needed.
fn storeTypeIntoType(value: IR, from_type: Type, to_type: Type) EvalExprError!ExprEvalResult {
    if (!std.meta.eql(from_type, to_type)) return reportError("Type {} does not fit into type {}");
    return ExprEvalResult{
        .t_type = to_type,
        .ir = value,
    };
}

fn evaluateExpr(environment: *Environment, decl: ast.Expression) EvalExprError!ExprEvalResult {
    switch (decl) {
        .vardecl => |vd| {
            // assert the variable is declared in this layer of environment first
            const decl_info = environment.get(vd.name.*) orelse return reportError("Variable not defined");

            // initialize the type (if it has not already been initialized)
            try decl_info.decl_type.initialize();

            // check if the initializer ir has already been created (eg a variable with no type specified will do this)
            const rir = decl_info.decl_type.initialized.ir orelse blk: {
                // create the initial value
                const initial_value = try evaluateExpr(environment, vd.expression.*);

                // compare the expected type (from decl_info) with the actual type (from initial_value) and store it
                const stored = try storeTypeIntoType(initial_value.ir, initial_value.t_type, decl_info.decl_type.initialized.t_type);

                break :blk try allocDupe(environment.alloc, stored.ir);
            };

            return ExprEvalResult{
                .t_type = .unit,
                .ir = switch (vd.vartype) {
                    .let => @panic("TODO let"),
                    .const_ => IR{ .vardecl = .{ .jsname = decl_info.jsname, .reassign = false, .initial = rir } },
                    .trigger => @panic("TODO trigger"),
                    .state => IR{ .vardecl_w = .{ .jsname = decl_info.jsname, .initial = rir } },
                },
            };
        },
        .function => |fnction| {
            std.debug.panic("TODO function", .{});
        },
        else => std.debug.panic("TODO .{}", .{@tagName(decl)}),
    }
    return ExprEvalResult{
        .t_type = .unit,
    };
}

const IR = union(enum) {
    vardecl: struct {
        reassign: bool,
        initial: *IR,
        jsname: []const u8,
    },
    vardecl_w: struct {
        initial: *IR,
        jsname: []const u8,
    },
    varget: []const u8,
    varset: struct {
        jsname: []const u8,
        newval: *IR,
    },
    varget_w: []const u8,
    varset_w: struct {
        jsname: []const u8,
        newval: *IR,
    },
    block: []IR,
    watchable: *IR,
    number: f64,
    string: []const u8,
    htmlelement,
    htmlattribute,
    t_type: Type, // only available at comptime

    fn print(ir: IR, out: anytype, indent: usize) @TypeOf(out).Error!void {
        switch (ir) {
            .vardecl => |vd| {
                try out.print("const {} = ", .{vd.jsname});
                try vd.initial.print(out, indent);
                try out.writeAll(";");
            },
            else => try out.print("ō.TODO(\"{}\")", .{@tagName(ir)}),
        }
    }
};
// we don't have to go too far at first, just do the simplest thing
// don't overdo it or this will never be completed

// this will be complicated:
// like it's not too bad but it is

// const counter = widget() {
// 	state counted: i54 = 0;
// 	return .div(
// 		:class="counter",
// 		.button("++", :onclick = fn() counted += 1),
// 		.span(" ", counted, " "),
// 		.button("--", :onclick = fn() counted -= 1),
// 	);
// }

// ->

// const counter_0 = () => {
// 	const $counted_1 = new ō.WatchablePrimitive(0);
// 	return ō.el("div", [
// 		ō.attr("class", "counter"),
// 		ō.el("button", ["++"], ō.attr("onclick", () => $counted_1.set($counted_1.value + 1))),
// 		ō.el("span", [" ", ō.tn($counted_1.watch(() => $counted_1.value)), " "]),
// 		ō.el("button", ["--"], ō.attr("onclick", () => $counted_1.set($counted_1.value - 1))),
// 	]);
// }

// ok what does this mean

// (vardecl "counter_0" (fn (block
//    (vardecl_w "$counted_1" (watchable 0))
//    (return (htmlelement "div"
//        (htmlattribute "class" "counter")
//        (htmlelement "button" "++" (htmlattribute "onclick" (fn (varset_w "$counted_1" (add (varget_w "$counted_1") 1)))))
//    ))
// )))

pub fn main() !void {
    const code = @embedFile("../tests/consistent2.ul");

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const parsed = try ast.parse(alloc, code, .File);

    const out = std.io.getStdOut().writer();

    var renv = Environment.init(alloc, null, .{ .catchReturns = false });
    defer renv.deinit();

    var resv = try evaluateExprs(&renv, parsed);
    try resv.ir.print(out, 0);
}
