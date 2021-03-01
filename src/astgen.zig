const ast = @import("uilang_parser");
const std = @import("std");
const Alloc = std.mem.Allocator;
const IR = @import("ir.zig").IR;
const Type = @import("type.zig").Type;
usingnamespace @import("help.zig");

//! ast → ir
//! (also does typechecking I guess)

// ok so namespace{}
// it has to make a type
// it does not analyze stuff until used
// rather than being placed in locally, it gets inserted into the code as a global constant
// (ir will have a second section for inserting constants I guess and then both will be written to the file, constants first)
// how does that work uuh

pub fn astToIR(alloc: *Alloc, file: ast.File) !IR {
    var renv = Environment.init(alloc, null, .{});
    defer renv.deinit();
    var resv = try evaluateExprs(&renv, file, .function);
    return resv.ir;
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

// wow this is bad. todo fix.
const LaterType = union(enum) {
    uninitialized_type: struct {
        env: *Environment,
        type_code: *ast.Expression,
        should_be_watchable: bool,
    },
    // in this case, the expression needs to be evaluated and then latertype will actually
    // be storing the result ir to be inserted with the vardecl
    uninitialized_expression: struct {
        env: *Environment,
        expression_code: *ast.Expression,
        should_be_watchable: bool,
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
                var tcopy = exprval.ir.t_type;
                if (unin.should_be_watchable) tcopy.watchable = true //
                else if (tcopy.watchable) unreachable; // watchable type used in nonwatchable variable
                lt.* = .{
                    .initialized = .{ .ir = null, .t_type = tcopy },
                };
            },
            .uninitialized_expression => |uninexpr| {
                const env = uninexpr.env;
                const exprcode = uninexpr.expression_code;
                const exprval = try evaluateExpr(env, exprcode.*, .function);
                const allocdir = try allocDupe(env.alloc, exprval.ir);

                var tcopy = exprval.t_type;
                if (uninexpr.should_be_watchable) tcopy.watchable = true //
                else if (tcopy.watchable) unreachable; // watchable type used in nonwatchable variable

                lt.* = .{
                    .initialized = .{ .ir = allocdir, .t_type = tcopy },
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
        const should_be_watchable = switch (kind) {
            .const_ => false,
            .let => false,
            .state => true,
            .trigger => true,
        };
        return DeclInfo{
            .kind = kind,
            .decl_type = if (typev) |tv|
                LaterType{ .uninitialized_type = .{ .env = env, .type_code = tv, .should_be_watchable = should_be_watchable } }
            else
                LaterType{
                    .uninitialized_expression = .{
                        .env = env,
                        .expression_code = valuev,
                        .should_be_watchable = should_be_watchable,
                    },
                },
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
    // get the return type of this environment.
    // set by catching control flow and informing that layer about the type you have returned.
    fn returnType(env: Environment) Type {
        return .{ .watchable = false, .tkind = .unit };
    }
};

fn evalDeclArray(decls: []ast.Expression, env: *Environment) void {}

const ReportedError = error{Reported};
fn reportError(msg: []const u8) ReportedError {
    std.log.err("Error: {}", .{msg});
    return ReportedError.Reported;
}

const EvalExprResult = struct {
    t_type: Type,
    ir: IR,
    /// if this value can be assigned to
    assignable: bool,
};

const VarDeclInfo = struct { typxpr: ?*ast.Expression, dvxpr: *ast.Expression };
fn getVarDeclInfo(vd: ast.Vardecl) EvalExprError!VarDeclInfo {
    return switch (vd.initmode) {
        .ttype => |tt| switch (tt.texpr.*) {
            .assignop => |aop| blk: {
                if (aop.len != 3) return reportError("Too many equals signs. Expected var varname: type = value;");
                if (aop[1].op != .eq) return reportError("Expected `=` in var varname: type = value, not {}");
                break :blk .{ .typxpr = &aop[0]._, .dvxpr = &aop[2]._ };
            },
            else => return reportError("Missing `=` (Expected var varname: type = value)"),
        },
        .auto => |ao| .{ .typxpr = null, .dvxpr = ao.initv },
    };
}

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
            const vs = try getVarDeclInfo(vd.*);
            try env.declare(vd.name.*, vt, vs.typxpr, vs.dvxpr);
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
        .t_type = env.returnType(),
        .ir = IR{ .block = .{ .blockid = if (env.env_cfg.catches) |catches| catches.blkid else null, .body = resIR.toOwnedSlice() } },
    };
}

/// store ir value into to_type, coercing if needed.
fn storeTypeIntoType(value: EvalExprResult, to_type: Type) EvalExprError!EvalExprResult {
    const from_type = &value.t_type;

    if (from_type.watchable and !to_type.watchable) return reportError("TODO unwatch if allowed here");
    // if(to_type.watchable and !from_type.watchable) return reportError(""); // ok for now I guess idk

    if (from_type.tkind == .ct_number) {
        if (to_type.tkind == .int) {
            return EvalExprResult{
                .assignable = false, // @as(i64, somevar) = 25; is not ok.
                .t_type = to_type,
                .ir = value.ir,
            };
        }
    }
    if (!std.meta.eql(from_type.tkind, to_type.tkind)) {
        std.debug.warn("storing {} into {}\n", .{ from_type.*, to_type });
        return reportError("Type {} does not fit into type {}");
    }
    return EvalExprResult{
        // @as(i64, (var that is i64)) = 25; // is that ok? probably not
        .assignable = false,
        .t_type = to_type,
        .ir = value.ir,
    };
}

// TODO rather than having a seperate explicit unwatch, make this a union(enum) and put it as a property in function
const ExecutionMode = enum {
    function, // normal top to bottom run code
    widget, // weird watchable code
    explicit_unwatch,
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
                const vs = try getVarDeclInfo(vd.*);

                // create the initial value
                const initial_value = try evaluateExpr(env, vs.dvxpr.*, .explicit_unwatch);

                // compare the expected type (from decl_info) with the actual type (from initial_value) and store it
                const stored = try storeTypeIntoType(initial_value, decl_info.decl_type.initialized.t_type);

                break :blk try allocDupe(env.alloc, stored.ir);
            };

            switch (mode) {
                .function, .explicit_unwatch => switch (vd.vartype) {
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
            const fn_kind = @as(std.meta.Tag(@TypeOf(func.kind)), func.kind);
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
            if (std.mem.eql(u8, varbl.name.*, "i53")) {
                return EvalExprResult{
                    .assignable = false, // i53 = i25 // no
                    .t_type = .{ .tkind = .t_type, .watchable = false },
                    .ir = IR{
                        .t_type = .{ .watchable = false, .tkind = .{ .int = 53 } },
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
                .state, .trigger => if (!decl_info.decl_type.initialized.t_type.watchable) unreachable, // type is not watchable, see above
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
            if (opitms.len != 3) std.debug.panic("TODO support operator chains", .{});
            // if either value is watchable,
            // - if .widget => produce a watch expression. also unwrap sub-watch exprs if needed.
            // - if .function => unwatch(lhs) unwatch(rhs)

            var lhs = try evaluateExpr(env, opitms[0]._, mode);
            var rhs = try evaluateExpr(env, opitms[2]._, mode);

            switch (mode) {
                .function => {
                    // what happens if the result location is the same as the location of an argument?
                    // does it have to make a copy or something? do things break if I set
                    // things in @resultLocation() and then use the second argument? idk
                    lhs = try unwatch(env, lhs);
                    rhs = try unwatch(env, rhs);
                },
                .explicit_unwatch => if (lhs.t_type.watchable or rhs.t_type.watchable) {
                    return reportError("Explicit unwatch statements are required here because this context" ++
                        " looks like it is widget but is actually function.");
                },
                .widget => {
                    if (lhs.t_type.watchable or rhs.t_type.watchable) {
                        std.debug.panic("TODO support watchable lhs/rhs of add in functions", .{});
                    }
                },
            }

            // TODO determine a common type based on the types of l and rhs
            const commontype = Type{ .watchable = false, .tkind = .{ .int = 53 } };

            lhs = try storeTypeIntoType(lhs, commontype);
            rhs = try storeTypeIntoType(rhs, commontype);

            return EvalExprResult{
                .assignable = false,
                .t_type = commontype,
                .ir = IR{
                    .math = .{
                        .lhs = try allocDupe(env.alloc, lhs.ir),
                        .rhs = try allocDupe(env.alloc, rhs.ir),
                        .op = switch (opitms[1].op) {
                            .plus => .add,
                            .minus => .sub,
                        },
                    },
                },
            };
            // ? debug safety check that the operation did not overflow
            //   maybe
        },
        .assignop => |opitms| {
            // huh operators don't have a type yet
            // operators will need to be rethought in resyn
            // I want that thing to get the remainder of union items idk
            if (opitms.len != 3) return reportError("Cannot repeat assignop");
            switch (mode) {
                .widget => return reportError("cannot assignop in widget context"),
                .function => {},
                .explicit_unwatch => {}, // probably a block, ok I think, maybe it was a mistake adding explicit_unwatch.
            }

            const lhs = try evaluateExpr(env, opitms[0]._, mode);
            if (!lhs.assignable) return reportError("lhs is not assignable");

            // maybe there can be (set_variable_value) and (set_property) or something idk

            switch (opitms[1].op) {
                .eq => {
                    // maybe unwatch itself should error if this is a function context with explicit unwatch set
                    // otherwise this isn't very useful
                    // ok todo future that
                    const rhs = try storeTypeIntoType(try unwatch(env, try evaluateExpr(env, opitms[2]._, mode)), lhs.t_type);

                    return EvalExprResult{
                        .assignable = false,
                        .t_type = .{ .watchable = false, .tkind = .unit },
                        .ir = .{
                            .assign = .{
                                .lval = try allocDupe(env.alloc, lhs.ir),
                                .rhs = try allocDupe(env.alloc, rhs.ir),
                                .watchable = lhs.t_type.watchable,
                            },
                        },
                    };
                },
                .pleq => {
                    // IR: (assign :left (plus :left :right))
                    // return IR.parse("(assign :left (plus :left :right))", .{.left = opitms[0]._, .right = opitms[2]._});
                    // left has to be assignable
                    // if right is watchable, it must be unwatched (.value)

                    // how about doing plusop and eqop seperately first
                    // plusop needs to unwatch lhs and rhs only if the mode is normal
                    // if the mode is watchable, plusop needs to watch the needed things in lhs/rhs and
                    // make a (watch (fn a + b) [$a, $b])

                    // since assignop is only in normal contexts, there is no issue with this and the value just has to be unwatched if watchable
                    // ok do assignop first
                    const rhs = try unwatch(env, try evaluateExpr(env, opitms[2]._, mode));

                    // hopefully it is okay to evaluate lhs twice I hope
                    // no it isn't.
                    //     array[(a++, a)] += 3;
                    // you can't duplicate that
                    //     const lhs_unwatched = try unwatch(lhs);
                    // not ok
                    // uuh how to generate code for this
                    //     array[(a++, a)] += 3
                    //    _1 = (a++, a)
                    //    _2 = array
                    //    _2[_1] = _1 + _3
                    // we have to generate that
                    // ? how though

                    // (assign lhs.ir (plus (unwatch lhs.ir) rhs.ir))
                    // how to not duplicate lhs? hopefully assignable lhs s will be allowed to be duplicated

                    @panic("TODO figure out how to not duplicate lhs. maybe do = first I guess idk");
                },
                else => std.debug.panic("TODO {} operator", .{@tagName(opitms[1].op)}),
            }
        },
        .builtinexpr => |bxpr| {
            if (std.mem.eql(u8, bxpr.name.*, "import_std")) {
                if (bxpr.args.len != 0) return reportError("bad args");
                return EvalExprResult{
                    .assignable = false,
                    .t_type = .{ .watchable = false, .tkind = .t_type },
                    .ir = .{
                        .t_type = .{
                            .watchable = false,
                            .tkind = .{
                                .namespace = .{},
                            },
                        },
                    },
                };
            }
            return reportError("Unknown builtinexpr {}");
        },
        else => std.debug.panic("TODO .{}", .{@tagName(decl)}),
    }
}

// wrap an evalexprresult in an unwatch if needed
fn unwatch(env: *Environment, eer: EvalExprResult) EvalExprError!EvalExprResult {
    if (!eer.t_type.watchable) return eer;

    var ttcopy = eer.t_type;
    ttcopy.watchable = false;

    return EvalExprResult{
        .assignable = false,
        .t_type = ttcopy,
        .ir = IR{ .unwatch = try allocDupe(env.alloc, eer.ir) },
    };
}

// eg blk: (expr) will run this with .{.blk_name = "blk"} eg
// so you know it catches `break :blk`
fn evaluateExprInNewEnv(env: *Environment, decl: ast.Expression, block_opts: Environment.EnvironmentConfig, mode: ExecutionMode) EvalExprError!EvalExprResult {
    var thisenv = Environment.init(env.alloc, env, block_opts);
    switch (decl) {
        .block => |bk| {
            return try evaluateExprs(&thisenv, bk.decls, mode);
        },
        else => {
            const rv = try evaluateExpr(&thisenv, decl, mode);

            if (block_opts.catches) |catches| {
                // const resIR = IR{ .block = .{ .blkid = catches.blkid } };
                // return rv;
                // uuh what?
                // fn() return 5
                // ok that's an example
                // so uuh
                // ??
                // return 5 will be a noreturn
                // but it will set the block's return value to i53
                // so this has to make a block ok
                // and then the printer can optimize the block away
                const return_instr = IR{ .breakv = .{ .blkid = catches.blkid, .value = try allocDupe(env.alloc, rv.ir) } };

                const res_body = try std.mem.dupe(env.alloc, IR, &[_]IR{return_instr});

                const resIR = IR{ .block = .{ .blockid = catches.blkid, .body = res_body } };

                // TODO tell env about rv's return value

                return EvalExprResult{ .assignable = false, .t_type = thisenv.returnType(), .ir = resIR };
            } else {
                return rv;
            }
        },
    }
}
