const std = @import("std");
const ast = @import("uilang_parser");
const Alloc = std.mem.Allocator;

const ID = usize;

// in case this is ever made multithreaded, each thread will need a new starting point for this global_id (c pthread_self() * like std.math.maxInt(u50))
threadlocal var global_id: ID = 0;

fn getNewID() usize {
    defer global_id += 1;
    return global_id;
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
        env: *Environment,
        type_code: *ast.Expression,
    },
    // in this case, the expression needs to be evaluated and then latertype will actually
    // be storing the result ir to be inserted with the vardecl
    uninitialized_expression: struct {
        env: *Environment,
        expression_code: *ast.Expression,
    },
    in_progress: void,
    initialized: struct {
        t_type: Type,
        ir: ?*IR,
    },
    // TODO: if watchable, return a .kind = .watchable or whatever
    fn initialize(lt: *LaterType) EvalExprError!void {
        if (lt.* == .initialized) return;
        const copy = lt.*;
        lt.* = .in_progress;
        switch (copy) {
            .uninitialized_type => |unin| {
                const env = unin.env;
                const type_code = unin.type_code;
                const exprval = try evaluateExpr(env, type_code.*, .function);
                if (exprval.t_type.tkind != .t_type) {
                    // uh oh
                    std.debug.warn("Got {}\n", .{@tagName(exprval.t_type.tkind)});
                    std.debug.warn("Got {}\n", .{exprval});
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
                const env = uninexpr.env;
                const exprcode = uninexpr.expression_code;
                const exprval = try evaluateExpr(env, exprcode.*, .function);
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
                LaterType{ .uninitialized_type = .{ .env = env, .type_code = tv } }
            else
                LaterType{ .uninitialized_expression = .{ .env = env, .expression_code = valuev } },
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
    env_cfg: EnvironmentConfig,
    // deferStatements: unmanagedArrayList(something), for if we ever do defer statements
    _variables: std.StringHashMap(DeclInfo),
    const ControlFlowCatches = union(enum) {
        /// if this env layer catches return statements (eg fn() {if(true) {return}}) returns from the fn
        returns: void,
        /// if this env layer catches break statements (eg while(true) {break;}). if a label is provided,
        /// only catches for break statements with that label (eg blk: {break :blk 0;})
        breaks: ?[]const u8,
    };
    const CatchesConfig = struct { match: ControlFlowCatches, blkid: usize };
    const EnvironmentConfig = struct {
        /// what control flow statements this environment catches
        catches: ?CatchesConfig = null,
        /// if control flow can pass up through this layer (eg blk: fn() break :blk) is not allowed
        allow_cf_passthrough: bool = true,
    };
    fn catchControlFlow(me: Environment, ccc: ControlFlowCatches) ?CatchesConfig {
        if (me.env_cfg.catches) |ctchs| {
            if (std.meta.eql(ctchs.match, ccc)) return ctchs;
        }
        if (me.env_cfg.allow_cf_passthrough) {
            if (me.parent) |parent| return parent.catchControlFlow(ccc); // enforce tail call for when zig disallows recursion
            return null;
        }
        return null;
    }
    fn init(alloc: *Alloc, parent: ?*Environment, config: EnvironmentConfig) Environment {
        return Environment{
            .alloc = alloc,
            .parent = parent,
            .env_cfg = config,
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

const Type = struct {
    /// if this value is watchable
    watchable: bool,
    tkind: union(enum) {
        empty, // there is no value. like `never` in typescript.
        unit, // there is one value.
        t_type, // represents a type. only available at comptime.
        func, // any function. will include args and return type in the future.
        int: u8, // an integer, max i53
        uint: u8, // a uint, max u53 (higher requires some >>> mess)
        float, // a f64.
        ct_number, // a string number. must be casted into a runtime value to use.
        html, // a html element.
        attr, // a html attribute. maybe include more information in the future incl the type of attribute.
        string, // a string
    },
};
const EvalExprResult = struct {
    t_type: Type,
    ir: IR,
    /// if this value can be assigned to
    assignable: bool,
};

const EvalExprError = ReportedError || error{OutOfMemory};
// if we do result location stuff, we can skip a bit
// resultLocation: struct {result: Type, outname: []const u8}
// -> struct {result: Type}
fn evaluateExprs(env: *Environment, decls: []ast.Expression, mode: ExecutionMode) EvalExprError!EvalExprResult {
    // 1: hoist vardecls
    for (decls) |decl| switch (decl) {
        .vardecl => |vd| {
            // add decl to env
            const vt: DeclInfo.DeclKind = switch (vd.vartype) {
                .let => .let,
                .const_ => .const_,
                .state => .state,
                .trigger => .trigger,
            };
            try env.declare(vd.name.*, vt, if (vd.ttype) |tt| tt.expression else null, vd.expression);
        },
        else => {}, // ok
    };

    var resIR = std.ArrayList(IR).init(env.alloc);

    // 2: evaluate expressions
    var is_unreachable = false;
    for (decls) |decl| {
        if (is_unreachable) return reportError("Unreachable");
        const resultValue = try evaluateExpr(env, decl, mode);
        // if res type is empty, check that this is at the end, else found unreachable code
        switch (resultValue.t_type.tkind) {
            // TODO
            // if empty, the block return value should be set to empty. this will say that the block never returns.
            // if the block's return value is set already, this won't happen.
            // this is how control flow analysis will work. very simple.
            .empty => is_unreachable = true,
            .unit => {},
            else => return reportError("Expected unit or empty"),
        }
        try resIR.append(resultValue.ir);
    }

    // if any of those decls return, the info will be written to env
    // this can be used to determine the return type
    return EvalExprResult{
        .assignable = false, // the result of a block cannot be assigned to
        // TODO: determine the return type based on what value(s) are returned
        .t_type = .{ .tkind = .unit, .watchable = false },
        .ir = IR{ .block = .{ .blockid = if (env.env_cfg.catches) |catches| catches.blkid else null, .body = resIR.toOwnedSlice() } },
    };
}

/// store ir value into to_type, coercing if needed.
fn storeTypeIntoType(value: IR, from_type: Type, to_type: Type) EvalExprError!EvalExprResult {
    if (from_type.tkind == .ct_number) {
        if (to_type.tkind == .int) {
            return EvalExprResult{
                .assignable = false, // @as(i64, somevar) = 25; is not ok.
                .t_type = to_type,
                .ir = value,
            };
        }
    }
    if (!std.meta.eql(from_type, to_type)) return reportError("Type {} does not fit into type {}");
    return EvalExprResult{
        // @as(i64, (var that is i64)) = 25; // is that ok? probably not
        .assignable = false,
        .t_type = to_type,
        .ir = value,
    };
}

const ExecutionMode = enum {
    function, // normal top to bottom run code
    widget, // weird watchable code
};

fn evaluateExpr(env: *Environment, decl: ast.Expression, mode: ExecutionMode) EvalExprError!EvalExprResult {
    switch (decl) {
        // TODO note that constants cannot be set to a watchable value
        // TODO make watchability part of the type system
        // {const: bool, watchable: bool, typev: union(enum) {…}}
        .vardecl => |vd| {
            // make sure the variable is declared in this layer of env first
            const decl_info = env.get(vd.name.*) orelse return reportError("Variable not defined");

            // initialize the type (if it has not already been initialized)
            try decl_info.decl_type.initialize();

            // check if the initializer ir has already been created (eg a variable with no type specified will do this)
            const rir = decl_info.decl_type.initialized.ir orelse blk: {
                // create the initial value
                const initial_value = try evaluateExpr(env, vd.expression.*, .function);

                // compare the expected type (from decl_info) with the actual type (from initial_value) and store it
                const stored = try storeTypeIntoType(initial_value.ir, initial_value.t_type, decl_info.decl_type.initialized.t_type);

                break :blk try allocDupe(env.alloc, stored.ir);
            };

            switch (mode) {
                .function => switch (vd.vartype) {
                    .state => return reportError("State is not allowed in a function context"),
                    .trigger => return reportError("Let is not allowed in a function context"),
                    else => {},
                },
                .widget => switch (vd.vartype) {
                    .let => return reportError("Let is not allowed in a widget context"),
                    else => {},
                },
            }

            return EvalExprResult{
                .assignable = false, // (var a = 5) = 2 // yeah no
                .t_type = .{ .tkind = .unit, .watchable = false },
                .ir = switch (vd.vartype) {
                    .let => @panic("TODO let"),
                    .const_ => IR{ .vardecl = .{ .jsname = decl_info.jsname, .reassign = false, .initial = rir } },
                    .trigger => @panic("TODO trigger"),
                    .state => IR{ .vardecl_w = .{ .jsname = decl_info.jsname, .initial = rir } },
                },
            };
        },
        .function => |func| {
            const fn_kind = @as(@TagType(@TypeOf(func.kind)), func.kind);
            if (func.args.len > 0) @panic("TODO fn args");

            // fn kind determines some stuff
            // a widget fn puts control flow and stuff in special stuff
            // fn() {
            //    state counted = 0;
            //    counted += 1; // this is fine
            // }
            // widget() {
            //    state counted = 0;
            //    counted += 1; // not ok I guess idk
            //    const x = counted + 1; // does this change with counted? I think so?
            // }

            const ccfg = Environment.CatchesConfig{ .match = .returns, .blkid = getNewID() };
            const bodyv = try evaluateExprInNewEnv(env, func.expression.*, .{
                .catches = ccfg,
                .allow_cf_passthrough = false,
            }, switch (fn_kind) {
                .function => .function,
                .widget => .widget,
            });
            // bodyv.t_type is the fn return type. ensure that matches the expected provided return type.

            return EvalExprResult{
                .assignable = false, // (fn() 2) = 5; // nope
                .t_type = .{ .tkind = .func, .watchable = false },
                .ir = IR{
                    .func = .{
                        .body = try allocDupe(env.alloc, bodyv.ir),
                    },
                },
            };
        },
        .block => |block| {
            var thisenv = Environment.init(env.alloc, env, .{});

            return try evaluateExprs(&thisenv, block.decls, mode);
        },
        .variable => |varbl| {
            // how to decide if the result of this expression should be watched or not?
            //     a + 1
            // in a fn, that should just do a.value + 1
            // in a widget though, that should probably do a.watch(nv => nv + 1)
            // ok
            //     const somevalue = a + 1;
            // that should uuh
            // not be allowed?
            // (in widget, idk about fn)
            //     .div(somevalue + 1)
            // that should somevalue.watch(nv => nv + 1)
            //     somevalue += 1;
            // in widget, that should not be allowed
            //     fn() somevalue + 1
            // that should somevalue.value + 1 because it's in a normal fn now, not a widget anymore
            if (std.mem.eql(u8, varbl.name.*, "i53")) {
                return EvalExprResult{
                    .assignable = false, // i53 = i25 // no
                    .t_type = .{ .tkind = .t_type, .watchable = false },
                    .ir = IR{
                        .t_type = .{ .watchable = false, .tkind = .{ .int = 54 } },
                    },
                };
                // TODO not this. maybe put types in a part of the stdlib you can
                // usingnamespace or something. whatever it is, not this.
            }

            const decl_info: *DeclInfo = env.get(varbl.name.*) orelse return reportError("Variable not defined");

            try decl_info.decl_type.initialize();

            switch (decl_info.kind) {
                // lets/constants are not watchable. if a constant is set to a watchable value, it should be
                // .value'd
                // watch a = 25;
                // fn() const b = a;
                // => () => {const b = a.value;}
                // if you want to keep the watchable, use `bind`
                // watch a = 25;
                // bind b = a;
                .let, .const_ => if (decl_info.decl_type.initialized.t_type.watchable) unreachable, // type is watchable, see above
                // state/trigger should make the type watchable
                // watch a: i54 = 25;
                // that should make the type watchable<i54>
                .state, .trigger => if (decl_info.decl_type.initialized.t_type.watchable) unreachable, // type is not watchable, see above
            }

            return EvalExprResult{
                .assignable = switch (decl_info.kind) {
                    .const_ => false,
                    .let => true,
                    .state, .trigger => true,
                },
                .t_type = decl_info.decl_type.initialized.t_type,
                .ir = IR{ .varget = decl_info.jsname },
            };
        },
        .number => |numv| {
            return EvalExprResult{
                .assignable = false, // 25 = 54; // no
                .t_type = .{ .tkind = .ct_number, .watchable = false },
                .ir = IR{ .number = numv.* },
            };
        },
        .returnstatement => |rs| {
            const expr = rs.expression;
            const topenv = env.catchControlFlow(.returns) orelse return reportError("No control flow found");
            const resultir = try evaluateExpr(env, expr.*, mode);
            // TODO write the return type to topenv so it knows. (catchControlFlow will have to return the env and accept a *Env ptr)
            // or if a type is already written, confirm that this matches that.
            // TODO

            return EvalExprResult{
                .assignable = false, // (return 5) = 2; // no
                .t_type = .{ .tkind = .empty, .watchable = false },
                .ir = IR{ .breakv = .{ .value = try allocDupe(env.alloc, resultir.ir), .blkid = topenv.blkid } },
            };
        },
        .htmlelement => |he| {
            var argsAL = std.ArrayList(IR).init(env.alloc);
            for (he.parens.items) |pitm| {
                const argv = try evaluateExpr(env, pitm, mode);
                switch (argv.t_type.tkind) {
                    .html, .attr, .string, .int => {},
                    else => return reportError("Unsupported type here {}"),
                }
                try argsAL.append(argv.ir);
            }
            return EvalExprResult{
                .assignable = false, // .div() = .h1(); // no
                .t_type = .{ .tkind = .html, .watchable = false },
                .ir = IR{ .html = .{ .tag = he.tag.*, .args = argsAL.toOwnedSlice() } },
            };
        },
        .htmlattribute => |ha| {
            const val = try evaluateExpr(env, ha.expression.*, mode);
            if (std.mem.startsWith(u8, ha.identifier.*, "on")) switch (val.t_type.tkind) {
                .func => {},
                else => return reportError("Unsupported type here {}"),
            } else switch (val.t_type.tkind) {
                .string => {},
                else => return reportError("Unsupported type here {}"),
            }
            return EvalExprResult{
                .assignable = false, // :class="hi" = 2; // no
                .t_type = .{ .tkind = .attr, .watchable = false },
                .ir = IR{ .attr = .{ .name = ha.identifier.*, .value = try allocDupe(env.alloc, val.ir) } },
            };
        },
        .string => |str| {
            var resTxt = std.ArrayList(u8).init(env.alloc);
            for (str.bits) |bit| switch (bit) {
                .string => |txt| try resTxt.appendSlice(txt),
                .escape => unreachable, // TODO string escapes
            };
            return EvalExprResult{
                .assignable = false,
                .t_type = .{ .tkind = .string, .watchable = false },
                .ir = IR{ .string = resTxt.toOwnedSlice() },
            };
        },
        .plusminusop => |opitms| {
            for (opitms) |opitm| {
                std.debug.warn("opitm: {}\n", .{opitm});
            }
            std.debug.panic("plusminusop produced even though {} len", .{opitms.len});
        },
        .assignop => |opitms| {
            // huh operators don't have a type yet
            // operators will need to be rethought in resyn
            // I want that thing to get the remainder of union items idk
            if (opitms.len != 3) return reportError("Cannot repeat assignop");
            if (mode == .widget) return reportError("Cannot assignop in widget context");
            switch (opitms[1].op) {
                .pleq => {
                    // IR: (assign :left (plus :left :right))
                    // return IR.parse("(assign :left (plus :left :right))", .{.left = opitms[0]._, .right = opitms[2]._});
                    // left has to be a value that can be assigned to, eg a watchable or a var or something
                    // invalid:
                    //     (blk: {break :blk watchable}) = 2;
                    //     // blk will turn the value into .constant
                    // valid:
                    //     watchable = 2;
                    //     // the variable is .watchable
                    // uh oh, what if the .watchable is constant
                    // idk this is weird and not structured right
                    @panic("TODO +=");
                },
                else => std.debug.panic("TODO {} operator", .{@tagName(opitms[1].op)}),
            }
        },
        else => std.debug.panic("TODO .{}", .{@tagName(decl)}),
    }
}

// eg blk: (expr) will run this with .{.blk_name = "blk"} eg
// so you know it catches `break :blk`
fn evaluateExprInNewEnv(env: *Environment, decl: ast.Expression, block_opts: Environment.EnvironmentConfig, mode: ExecutionMode) EvalExprError!EvalExprResult {
    switch (decl) {
        .block => |bk| {
            var thisenv = Environment.init(env.alloc, env, block_opts);

            return try evaluateExprs(&thisenv, bk.decls, mode);
        },
        else => {
            var thisenv = Environment.init(env.alloc, env, block_opts);

            const rv = try evaluateExpr(&thisenv, decl, mode);

            if (block_opts.catches) |catches| {
                // const resIR = IR{ .block = .{ .blkid = catches.blkid } };
                // return rv;
                return reportError("TODO support evaluateExprInNewEnv non-block with catches");
            } else {
                return rv;
            }
        },
    }
}

// {
//   if(one) {if(two) return 1;}
//   return 2;
// }
//
// ok idk
// (() => {
//    if(one) (() => {if(two) return 1)}();
//    return 2;
// })()
//
// for now would it be bad to
// (block blockid
//    (breakblk *blockiid)
// )
// note that breakblk can't break up above a function line
// (block blockid (fn (breakblk *blockid))) is not ok and will error.
// and then handle it in codegen?
// sounds ok
// same with defers.
// defers run before every breakblk or before the end of the block
// I guess we can put a default breakblk at the end of each block,
// no implicit breakblk void

// ok with watchable is it has to be finally wrapped at the use site
// so .div (somefn(somevalue + 1))
// how does that work?
// well: if somefn accepts a watchable arg, the watchable is wrapped
// at the level of ō.watch(() => somevalue.value + 1, [somevalue])
// if somefn does not have a watchablae arg though
// ō.watch(somefn(somevalue.value + 1), [somevalue])
// neat! also, .div always accepts a watchable arg so there's no
// chance it will be dumb and extend the watch too high up
// this is simple and this is the exact reason for making a language
// rather than messing with typescript and babel transformations
// this is actually getting somewhere wow amazing

// !!
// outer_block: {
//   inner_block: {
//     console.log('1');
//     break outer_block; // breaks out of both inner_block and outer_block
//     console.log(':-('); // skipped
//   }
//   console.log('2'); // skipped
// }
// javascript has labeled blocks
// just have to pass the return value through an unnamed variable
// also, defer statements can be inserted before the break

// so::
// {
//   if(one) {if(two) return 1;}
//   return 2;
// }
// ->
// var returnvalue_2;
// outerblk_1: {
//   if(one) {if(two) {returnvalue_2 = 1; break outerblk_1;}}
//   return 2;
// }
// returnvalue_2; // huh. there's no way to do this well really. this will require a codegen mess.

// it may be better to do this the zir way
// idk what that is but it's like instead of a(b(c)) it's 1 = a; 2 = b(1); 3 = c(2);
// that makes some codegen easier and some harder
// eg blocks require setting a variable to return a value
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
    html: struct { tag: []const u8, args: []IR },
    attr: struct { name: []const u8, value: *IR },
    breakv: struct { value: *IR, blkid: usize },
    block: struct { blockid: ?usize, body: []IR },
    func: struct {
        body: *IR,
    },
    // to support bigints, this is []const u8 rather than f64
    number: []const u8,
    string: []const u8,
    /// represents a watchable ir bit
    /// $watchablevar + 1
    /// ::: (watchable $watchablevar (fn (varget_w $watchablevar))) + 1
    /// ::: (watchable $watchablevar (fn (add (varget_w $watchablevar) (1))))
    /// but what if instead of this, @typeOf(varget_w $watchablevar) was .watchable = true
    /// and instead of watchable being ir, callers knew
    /// ok tbh I have no idea what I'm doing
    /// the reason for putting it in ir is so it can codegen
    /// because otherwise there's no way to codegen it
    /// so I do think watchable is a good idea I guess idk
    /// maybe fn is unnecessary, ir can do that itself
    watchable: struct {
        dependencies: []IR, // struct{dependenc: IR, mapnme: []const u8}?
        value: *IR,
    },
    htmlelement,
    htmlattribute,
    t_type: Type, // only available at comptime

    // there should be a version of this for expressions because not everything needs to be like this
    // if the thing can be printed as an expression, print it as one
    // that is actually hard to find out though. return .div(blk: {break :blk 2}); eg
    fn print(ir: IR, out: anytype, indent: usize, write_to: ?usize, return_blkid: usize) @TypeOf(out).Error!void {
        const idnt = IndentWriter{ .count = indent };
        switch (ir) {
            // .vardecl => |vd| {
            //     // TODO change this to only work inside blocks
            //     // otherwise expressions shouldn't end with ;
            //     try out.print("const {} = ", .{vd.jsname});
            //     try vd.initial.print(out, indent);
            //     try out.writeAll(";");
            // },
            .block => |blk| {
                if (blk.blockid) |blkid| {
                    try out.print("{}var _{}_ = undefined;\n", .{ idnt, blkid });
                    try out.print("{}_{}_blk_: {{\n", .{ idnt, blkid });
                }
                const nidnt = if (blk.blockid) |_| indent + 1 else indent;
                for (blk.body) |blkir| {
                    _ = try print(blkir, out, nidnt, null, return_blkid);
                }
                if (blk.blockid) |_| try out.print("{}}}\n", .{idnt});
                if (write_to) |wt| {
                    if (blk.blockid) |blkid| try out.print("{}var _{}_ = _{}_;\n", .{ idnt, wt, blkid }) // zig fmt
                    else try out.print("{}var _{}_ = undefined;\n", .{ idnt, wt });
                }
            },
            .vardecl => |vd| {
                const initialout = getNewID();
                try print(vd.initial.*, out, indent, initialout, return_blkid);
                try out.print("{}var {} = _{}_;\n", .{ idnt, vd.jsname, initialout });
                if (write_to) |wt| try out.print("{}var _{}_ = undefined;\n", .{ idnt, wt });
            },
            .vardecl_w => |vd| {
                const initialout = getNewID();
                try print(vd.initial.*, out, indent, initialout, return_blkid);
                try out.print("{}var {} = ō.watchable(_{}_);\n", .{ idnt, vd.jsname, initialout });
                if (write_to) |wt| try out.print("{}var _{}_ = undefined;\n", .{ idnt, wt });
            },
            .number => |num| {
                if (write_to) |wt| try out.print("{}var _{}_ = {};\n", .{ idnt, wt, num });
            },
            .string => |str| {
                if (write_to) |wt| {
                    try out.print("{}var _{}_ = ", .{ idnt, wt });
                    try printJSString(str, out);
                    try out.writeAll(";\n");
                }
            },
            .varget => |vg| {
                if (write_to) |wt| try out.print("{}var _{}_ = {};\n", .{ idnt, wt, vg });
            },
            .func => |func| {
                if (write_to == null) {
                    // func has no side effects and so it can be safely discarded.
                    try out.print("{}// a function was unused here\n", .{idnt});
                    return;
                }
                try out.print("{}var _{}_ = () => {{\n", .{
                    idnt,
                    write_to.?,
                });
                switch (func.body.*) {
                    .block => |blk| for (blk.body) |blkir| {
                        _ = try print(blkir, out, indent + 1, null, blk.blockid orelse std.math.maxInt(usize));
                    },
                    else => {
                        const bodyresid = getNewID();
                        try print(func.body.*, out, indent + 1, bodyresid, std.math.maxInt(usize));
                        try out.print("{}    return _{}_;\n", .{ idnt, bodyresid });
                    },
                }
                try out.print("{}}};\n", .{idnt});
            },
            .breakv => |bv| {
                var wtv = getNewID();
                try print(bv.value.*, out, indent, wtv, return_blkid);
                if (bv.blkid == return_blkid) {
                    try out.print("{}return _{}_;\n", .{ idnt, wtv });
                } else {
                    try out.print("{}_{}_ = _{}_;\n", .{ idnt, bv.blkid, wtv });
                    try out.print("{}break _{}_blk;\n", .{ idnt, bv.blkid });
                }
            },
            .html => |hl| {
                var startID = global_id;
                for (range(hl.args.len)) |_| global_id += 1;
                for (hl.args) |hlarg, i| {
                    const hlID = startID + i;
                    try print(hlarg, out, indent, hlID, return_blkid);
                }
                if (write_to) |wt| {
                    try out.print("{}var _{}_ = ō.html(", .{ idnt, wt });
                    try printJSString(hl.tag, out);
                    for (range(hl.args.len)) |_, i| {
                        const rid = startID + i;
                        try out.writeAll(", ");
                        try out.print("_{}_", .{rid});
                    }
                    try out.writeAll(");\n");
                }
            },
            .attr => |ar| {
                var arid = getNewID();
                try print(ar.value.*, out, indent, arid, return_blkid);
                if (write_to) |wt| {
                    try out.print("{}var _{}_ = ō.attr(", .{ idnt, arid });
                    try printJSString(ar.name, out);
                    try out.print(", _{});\n", .{arid});
                }
            },
            else => {
                try out.print("{}", .{idnt});
                if (write_to) |wt| try out.print("var _{}_ = ", .{wt});
                try out.print("ō.TODO(\"{}\");\n", .{@tagName(ir)});
            },
        }
    }
};
const IndentWriter = struct {
    count: usize,
    pub fn format(idw: IndentWriter, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        for (range(idw.count)) |_| {
            try out.writeAll("    ");
        }
    }
};
fn range(max: usize) []const void {
    return @as([]const void, &[_]void{}).ptr[0..max];
}

pub fn printJSString(str: []const u8, out: anytype) !void {
    try out.writeByte('"');
    for (str) |char| switch (char) {
        ' '...'~' => try out.writeByte(char),
        '\n' => try out.writeAll("\\n"),
        else => try out.print("\\x{x:0<2}", .{char}),
    };
    try out.writeByte('"');
}
// we don't have to go too far at first, just do the simplest thing
// don't overdo it or this will never be completed

// this will be complicated:
// like it's not too bad but it is

// const counter = widget() {
// 	state counted: i53 = 0;
// 	return .div(
// 		:class="counter",
// 		.button("++", :onclick = fn() counted += 1),
// 		.span(" ", counted, " "),
// 		.button("--", :onclick = fn() counted -= 1),
// 	);
// }
// a += 5;
// void a += 5 in js?

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

    var renv = Environment.init(alloc, null, .{});
    defer renv.deinit();

    var resv = try evaluateExprs(&renv, parsed, .function);
    try resv.ir.print(out, 0, null, std.math.maxInt(usize));
}
