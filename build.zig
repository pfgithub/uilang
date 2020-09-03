const std = @import("std");
const Builder = std.build.Builder;

pub fn addParser(src: []const u8, comptime packageName: []const u8, exe: anytype, b: *Builder) void {
    // build and run src/parser.zig
    // save the output to zig-cache/
    const parser = b.addExecutable("parser_generator", "src/generator.zig");
    parser.setTarget(.{}); // this will run on your machine so it should be native arch
    parser.setBuildMode(.Debug); // debug is fast enough idk

    // now run the exe and write the output to zig-cache/parser_packagename.zig

    const pkgfile = "zig-cache/parser_" ++ packageName ++ ".zig";

    const runner = b.addSystemCommand(&[_][]const u8{ "zig-cache/bin/parser_generator", src, pkgfile });
    runner.step.dependOn(&parser.step);

    exe.step.dependOn(&runner.step);
    exe.addPackagePath(packageName, pkgfile);
}

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("dist_test", "src/dist_test.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    addParser("src/resyn.resyn", "resyn_parser", exe, b);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
