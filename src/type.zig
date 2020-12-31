const std = @import("std");

pub const Type = struct {
    /// if this value is watchable
    watchable: bool,
    tkind: union(enum) {
        empty, // there is no value. like `never` in typescript or `noreturn` in zig.
        unit, // there is one value.
        t_type, // represents a type. only available at comptime.
        func, // any function. will include args and return type in the future.
        int: u8, // an integer, max i53
        uint: u8, // a uint, max u52 (higher requires some >>> mess)
        float, // a f64.
        ct_number, // a string number. must be casted into a runtime value to use.
        html, // a html element.
        attr, // a html attribute. maybe include more information in the future incl the type of attribute.
        string, // a string
        namespace: *Namespace,
    },
    fn print(typ: Type, out: anytype) @TypeOf(out).Error!void {
        if (typ.watchable) try out.writeAll("Watchable<");
        switch (typ.tkind) {
            .int => |int| try out.print("i{}", .{int}),
            .uint => |uint| try out.print("u{}", .{uint}),
            .namespace => |ns| try out.print("namespace{…}"),
            else => |value| {
                if (@TypeOf(value) != void) @compileError("Expected void");
                try out.writeAll(@tagName(typ.tkind));
            },
        }
        if (typ.watchable) try out.writeAll(">");
    }
    pub fn format(typ: Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, out: anytype) !void {
        try typ.print(out);
    }
};

pub const Namespace = struct {
    name: ?[]const u8,
    decls: std.StringHashMap(LazilyEvaluatedDecl),
};

pub const LazilyEvaluatedDecl = struct {
    name: []const u8,
    value: union(enum) {
        // instead of this should the value be some kind of lazily
        // evaluated IR thing? probably not, this is fine
        // uninitialized: struct {
        //     env: *Environment,
        //     type_code: ?*ast.Expression,
        //     expression_code: *ast.Expression,
        //     should_be_watchable: bool,
        // },
        // in_progress,
        // initialized: struct {
        //     t_type: Type,
        //     ir: ?*IR,
        // },
        // pub fn initializeType(lt: *LaterType) void {}
        // pub fn initializeValue(lt: *LaterType) void {}
    },
};
