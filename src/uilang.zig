const std = @import("std");
const parser = @import("uilang_parser");

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
    res.writer().print("_{x}", .{uniqud});

    return res.toOwnedSlice();
}

const Type = struct {};

const DeclInfo = struct {
    const DeclKind = enum { const_, let, state, trigger };
    kind: DeclKind,
    decl_type: Type,
    jsname: []const u8,
    fn init(alloc: *Alloc, kind: VarKind, name: []const u8) !VarInfo {
        return VarInfo{
            .kind = kind,
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
    parent: ?*Environment,
    _variables: std.StringHashMap(DeclInfo),
};

pub fn main() !void {
    const code = @embedFile("../tests/consistent2.ul");

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.testing.expect(!gpalloc.deinit());

    var arena = std.heap.ArenaAllocator.init(&gpalloc.allocator);
    defer arena.deinit();

    const alloc = &arena.allocator;

    const parsed = try parser.parse(alloc, code, .File);

    const out = std.io.getStdOut().writer();

    for (parsed) |decl| {
        try out.print("decl: {}\n", .{decl});
    }
}
