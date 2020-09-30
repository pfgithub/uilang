const std = @import("std");
const ast = @import("uilang_parser");
const Alloc = std.mem.Allocator;
const help = @import("lib/args.zig");
const ArgsIter = help.ArgsIter;

const astgen = @import("astgen.zig");
const codegen = @import("codegen.zig");

const Config = struct {
    parsing_args: bool = true,
    out_dir: ?[]const u8 = null,
    out_dir_loc: struct { idx: usize, sidx: usize } = undefined,
    _: []const Positional = &[_]Positional{},
};
const Positional = struct { text: []const u8, pos: usize };

pub const main = help.anyMain(exec);
pub fn exec(alloc: *std.mem.Allocator, ai: *ArgsIter, stdout: anytype) !void {
    var cfg = Config{};
    var positionals = std.ArrayList(Positional).init(alloc);
    while (ai.next()) |arg| {
        if (cfg.parsing_args) {
            if (std.mem.eql(u8, arg, "--")) {
                cfg.parsing_args = false;
                continue;
            }
            if (ai.readValue(arg, "--out-dir") catch return ai.err("Expected output directory")) |out_dir| {
                cfg.out_dir = out_dir;
                cfg.out_dir_loc = .{ .idx = ai.index, .sidx = ai.subindex };
                continue;
            }
            if (std.mem.startsWith(u8, arg, "-")) {
                return help.reportError(ai, ai.index, 0, "Bad arg. See --help");
            }
        }
        try positionals.append(.{ .text = arg, .pos = ai.index });
    }
    cfg._ = positionals.toOwnedSlice();

    if (cfg._.len == 0) return ai.err("Expected infile.ul, See --help") //
    else if (cfg._.len > 1) return help.reportError(ai, cfg._[1].pos, 0, "Not sure what this is for. See --help");

    if (cfg.out_dir == null) {
        return ai.err("Expected --out-dir=…, see --help");
    }

    const code = std.fs.cwd().readFileAlloc(alloc, cfg._[0].text, std.math.maxInt(usize)) catch |e| switch (e) {
        else => {
            try stdout.print("Error: {}\n", .{@errorName(e)});
            return help.reportError(ai, cfg._[0].pos, 0, "File read error.");
        },
    };
    std.fs.cwd().makePath(cfg.out_dir.?) catch |e| switch (e) {
        else => {
            try stdout.print("Error: {}\n", .{@errorName(e)});
            return help.reportError(ai, cfg.out_dir_loc.idx, cfg.out_dir_loc.sidx, "Directory read error.");
        },
    };
    var outdir = std.fs.cwd().openDir(cfg.out_dir.?, .{}) catch |e| switch (e) {
        else => {
            try stdout.print("Unexpected Error: {}\n", .{@errorName(e)});
            return help.reportError(ai, cfg.out_dir_loc.idx, cfg.out_dir_loc.sidx, "Unexpected directory read error.");
        },
    };
    defer outdir.close();

    try outdir.writeFile("index.html", @embedFile("js/index.html"));
    try outdir.writeFile("framework.js", @embedFile("js/framework.js"));

    const parsed = try ast.parse(alloc, code, .File);

    const resvir = try astgen.astToIR(alloc, parsed);
    {
        const out_uil_file = try outdir.createFile("bundle.js", .{});
        defer out_uil_file.close();

        const out = out_uil_file.outStream();
        try out.writeAll("// Generated code\n\n");
        try codegen.printRoot(resvir, out);
        // TODO this in uil rather than here obviously
        try out.writeAll("\nō.mount(document.body, counter_0())\n");
    }
}

test "" {
    std.meta.refAllDecls(@This());
}
