const std = @import("std");
const Builder = std.build.Builder;

pub fn addParser(src: []const u8, comptime packageName: []const u8, exe: anytype, b: *Builder) void {
    // build and run src/parser.zig
    // save the output to zig-cache/
    const parser = b.addExecutable("parser_generator", "src/parser_generator/generator.zig");
    parser.setTarget(.{}); // this will run on your machine so it should be native arch
    parser.setBuildMode(.Debug); // debug is fast enough idk
    parser.install();

    // now run the exe and write the output to zig-cache/parser_packagename.zig

    const pkgfile = "zig-cache/parser_" ++ packageName ++ ".zig";

    const runner = b.addSystemCommand(&[_][]const u8{ "zig-cache/bin/parser_generator", src, pkgfile });
    runner.step.dependOn(&parser.install_step.?.step);

    const fmt = b.addFmt(&[_][]const u8{pkgfile});
    fmt.step.dependOn(&runner.step);
    exe.step.dependOn(&fmt.step);

    exe.addPackagePath(packageName, pkgfile);
}

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();

    {
        const exe = b.addExecutable("dist_test", "src/parser_generator_test.zig");
        exe.setTarget(target);
        exe.setBuildMode(mode);
        addParser("src/parser_generator/resyn.resyn", "resyn_parser", exe, b);
        exe.install();

        const run_cmd = exe.run();
        run_cmd.step.dependOn(&exe.install_step.?.step);

        const run_step = b.step("test-parser-generator", "Test the parser generator");
        run_step.dependOn(&run_cmd.step);
    }

    {
        const pg = b.addExecutable("parser_generator", "src/parser_generator.zig");
        pg.setTarget(target);
        pg.setBuildMode(mode);
        pg.install();

        const pg_step = b.step("parser-generator", "Build the parser_generator");
        pg_step.dependOn(&pg.install_step.?.step);
    }

    {
        const uil = b.addExecutable("uilang", "src/uilang.zig");
        uil.setTarget(target);
        uil.setBuildMode(mode);
        addParser("src/uilang.resyn", "uilang_parser", uil, b);
        uil.install();

        const pg_step = b.step("uilang", "Build the uilang");
        pg_step.dependOn(&uil.install_step.?.step);
    }

    {
        const uil = b.addExecutable("uilang2", "src/uilang2.zig");
        uil.setTarget(target);
        uil.setBuildMode(mode);
        addParser("src/uilang.resyn", "uilang_parser", uil, b);
        uil.install();

        const pg_step = b.step("uilang2", "Build the uilang2");
        pg_step.dependOn(&uil.install_step.?.step);

        const run_cmd = uil.run();
        run_cmd.step.dependOn(&uil.install_step.?.step);

        const run_step = b.step("run-uilang2", "Test the uilang2");
        run_step.dependOn(&run_cmd.step);
    }
}
