const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("zart", .{
        .root_source_file = b.path("src/zart.zig"),
        .optimize = optimize,
        .target = target,
    });
    _ = mod;

    const lib = b.addStaticLibrary(.{
        .name = "zart",
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path("src/zart.zig"),
        .target = target,
        .optimize = optimize,
    });

    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).
    b.installArtifact(lib);

    // add check compile step
    check(b, b.step("check", "fast check compile"), .{ .target = target, .optimize = optimize });
    tests(b, b.step("test", "run unit test"), b.step("test:build", "build unit test"), .{ .target = target, .optimize = optimize });

    const bench = b.addExecutable(.{
        .name = "bench",
        .root_source_file = b.path("src/bench.zig"),
        .optimize = .ReleaseFast,
        .target = target,
    });
    b.installArtifact(bench);
    const bench_run = b.addRunArtifact(bench);
    const bench_step = b.step("bench", "Bench against std.StringHashMap()");
    bench_step.dependOn(&bench_run.step);

    const bench_build_step = b.step("bench:build", "Bench against std.StringHashMap()");
    bench_build_step.dependOn(&b.addInstallArtifact(bench, .{}).step);
}

/// fast compile check for easy development
fn check(b: *std.Build, step_check: *std.Build.Step, opts: struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
}) void {
    const lib = b.addTest(.{
        .root_source_file = b.path("src/zart.zig"),
        .target = opts.target,
        .optimize = opts.optimize,
    });
    step_check.dependOn(&lib.step);

    const pool_lib = b.addTest(.{
        .root_source_file = b.path("src/pool_allocator.zig"),
        .target = opts.target,
        .optimize = opts.optimize,
    });
    step_check.dependOn(&pool_lib.step);
}

fn tests(
    b: *std.Build,
    run_test_step: *std.Build.Step,
    build_test_step: *std.Build.Step,
    opts: struct {
        target: std.Build.ResolvedTarget,
        optimize: std.builtin.OptimizeMode,
    },
) void {
    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const test_filters = b.option([]const []const u8, "test-filter", "Only run test that match the filter");
    const lib_unit_tests = b.addTest(.{
        .name = "test",
        .root_source_file = b.path("src/pool_allocator.zig"),
        .test_runner = b.path("test_runner.zig"),
        .target = opts.target,
        .optimize = opts.optimize,
    });
    if (test_filters) |filters| {
        lib_unit_tests.filters = filters;
    }
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    run_test_step.dependOn(&run_lib_unit_tests.step);

    build_test_step.dependOn(&b.addInstallArtifact(lib_unit_tests, .{}).step);
}
