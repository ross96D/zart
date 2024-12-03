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
    tests(b, target, optimize);
    bench(b, target);
}

fn bench(b: *std.Build, target: std.Build.ResolvedTarget) void {
    const build_opts = b.addOptions();
    const hashopt = b.option(bool, "hashmap", "compile benchmark with hashmap") orelse false;
    const artopt = b.option(bool, "art", "compile benchmark with adaptive radix tree") orelse false;

    const name = if (!hashopt and !artopt) r: {
        build_opts.addOption(bool, "hashmap", true);
        build_opts.addOption(bool, "art", true);
        break :r "bench";
    } else r: {
        build_opts.addOption(bool, "hashmap", hashopt);
        build_opts.addOption(bool, "art", artopt);
        if (hashopt) {
            break :r "bench_hash";
        } else {
            break :r "bench_art";
        }
    };

    const bench_debug = b.option(bool, "bench-debug", "compile bench with debug") orelse false;
    const bench_compile = b.addExecutable(.{
        .name = name,
        .root_source_file = b.path("src/bench.zig"),
        .optimize = if (bench_debug) .Debug else .ReleaseFast,
        .target = target,
    });
    bench_compile.root_module.addOptions("build_opts", build_opts);

    b.installArtifact(bench_compile);
    const bench_run = b.addRunArtifact(bench_compile);
    const bench_step = b.step("bench", "Bench against std.StringHashMap()");
    bench_step.dependOn(&bench_run.step);

    const bench_build_step = b.step("bench:build", "Bench against std.StringHashMap()");
    bench_build_step.dependOn(&b.addInstallArtifact(bench_compile, .{}).step);
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
        .root_source_file = b.path("src/mem_pool.zig"),
        .target = opts.target,
        .optimize = opts.optimize,
    });
    step_check.dependOn(&pool_lib.step);
}

fn tests(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) void {
    const test_filters = b.option([]const []const u8, "test-filter", "Only run test that match the filter");
    { // build test for pool
        const run_test_step = b.step("test:pool", "run pool allocator unit test");
        const build_test_step = b.step("test:pool:build", "build pool allocator unit test");
        const lib_unit_tests = b.addTest(.{
            .name = "test_pool",
            .root_source_file = b.path("src/mem_pool.zig"),
            .test_runner = b.path("test_runner.zig"),
            .target = target,
            .optimize = optimize,
        });
        if (test_filters) |filters| {
            lib_unit_tests.filters = filters;
        }
        const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

        run_test_step.dependOn(&run_lib_unit_tests.step);

        build_test_step.dependOn(&b.addInstallArtifact(lib_unit_tests, .{}).step);
    }

    { // build test for pool
        const run_test_step = b.step("test", "run radix tree unit test");
        const build_test_step = b.step("test:build", "build radix tree unit test");
        const lib_unit_tests = b.addTest(.{
            .name = "test_pool",
            .root_source_file = b.path("src/zart.zig"),
            .test_runner = b.path("test_runner.zig"),
            .target = target,
            .optimize = optimize,
        });
        if (test_filters) |filters| {
            lib_unit_tests.filters = filters;
        }
        const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

        run_test_step.dependOn(&run_lib_unit_tests.step);

        build_test_step.dependOn(&b.addInstallArtifact(lib_unit_tests, .{}).step);
    }
}
