//! TODO refactor this benchmark

const std = @import("std");
const builtin = @import("builtin");
const art = @import("zart.zig");
const Art = art.Tree;
const art_test = art;
const build_opts = @import("build_opts");

const bench_log = if (std.debug.runtime_safety) std.debug.print else std.log.info;

const LevelsCollector = struct {
    const Self = @This();
    levels: std.ArrayList(usize),

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{ .levels = std.ArrayList(usize).init(allocator) };
    }
    pub fn deinit(self: LevelsCollector) void {
        self.levels.deinit();
    }

    pub fn print_statistics(self: Self) void {
        var total: usize = 0;
        var max: usize = 0;
        var min: usize = 1 << 63;

        const items = self.levels.items;
        for (items) |value| {
            total += value;
            if (max < value) {
                max = value;
            }
            if (min > value) {
                min = value;
            }
        }

        const avg: f64 = @as(f64, @floatFromInt(total)) / @as(f64, @floatFromInt(items.len));
        std.debug.print("avg: {d} max: {d} min: {d}\n\n", .{ avg, max, min });
    }

    fn for_each_yield(node: *const Art(usize).Node, label: u8, level: usize, _: usize, self: *Self) void {
        _ = label;
        if (node.leaf) |_| {
            self.levels.append(level) catch unreachable;
            // std.debug.print("level {d}\n", .{level});
        }
    }
};

fn bench(a: std.mem.Allocator, container: anytype, comptime appen_fn: anytype, comptime get_fn: anytype, comptime del_fn: anytype) !void {
    var lines = try art_test.readFileLines(a, "./testdata/words.txt");
    defer art_test.deinitLines(a, &lines);

    const doInsert_ = struct {
        fn func(line: [:0]const u8, linei: usize, _container: anytype, _: anytype, comptime U: type) anyerror!void {
            _ = U;
            const r = appen_fn(_container, line, linei);
            if (@typeInfo(@TypeOf(r)) == .ErrorUnion) {
                _ = try r;
            }
        }
    }.func;
    var timer = try std.time.Timer.start();
    _ = try art_test.eachLineDo(doInsert_, lines, container, null, usize);
    const t1 = timer.read();

    {
        const f = try std.fs.openFileAbsolute("/proc/self/status", .{});
        const data = try f.readToEndAlloc(a, 1 << 31);
        defer a.free(data);
        if (@TypeOf(container) == *Art(usize)) {
            std.debug.print("\nArt memory\n{s}\n\n", .{parseStatus(data)});
        } else {
            std.debug.print("\nStringHashMap memory\n{s}\n\n", .{parseStatus(data)});
        }
    }

    const doSearch = struct {
        fn func(line: [:0]const u8, linei: usize, _container: anytype, _: anytype, comptime U: type) anyerror!void {
            _ = U;
            _ = linei;
            if (@TypeOf(_container) == *Art(usize)) {
                const result = get_fn(_container, line);
                try std.testing.expect(result != null);
            } else {
                const result = get_fn(_container.*, line);
                try std.testing.expect(result != null);
            }
        }
    }.func;

    timer.reset();
    _ = try art_test.eachLineDo(doSearch, lines, container, null, usize);
    const t2 = timer.read();

    if (@TypeOf(container) == *Art(usize)) {
        var t = @as(*Art(usize), container);

        var collector = LevelsCollector.init(a);
        defer collector.deinit();
        t.for_each(&collector, LevelsCollector.for_each_yield);
        collector.print_statistics();
    } else {
        // std.debug.print("is not art(usize) instead is {s}\n", .{@typeName(@TypeOf(container))});
    }

    const doDelete = struct {
        fn func(line: [:0]const u8, linei: usize, _container: anytype, _: anytype, comptime U: type) anyerror!void {
            _ = U;
            _ = linei;
            if (@TypeOf(_container) == *Art(usize)) {
                const result = del_fn(_container, line);
                try std.testing.expect(result != null);
            } else {
                const result = del_fn(_container, line);
                try std.testing.expect(result);
            }
        }
    }.func;

    timer.reset();
    _ = try art_test.eachLineDo(doDelete, lines, container, null, usize);
    const t3 = timer.read();

    std.debug.print("insert {}ms, search {}ms, delete {}ms combined {}ms\n", .{ t1 / 1000000, t2 / 1000000, t3 / 1000000, (t1 + t2 + t3) / 1000000 });
    // try art_test.free_keys(container);
}

fn debug_allocator_stats(allocator: std.testing.FailingAllocator) []u8 {
    return std.fmt.allocPrint(std.heap.page_allocator, "allocations: {d} deallocations: {d} allocated_bytes: {d} freed_bytes: {d}", .{
        allocator.allocations,
        allocator.deallocations,
        allocator.allocated_bytes,
        allocator.freed_bytes,
    }) catch unreachable;
    // std.debug.print("", .{});
}

/// bench against StringHashMap
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    if (build_opts.hashmap) {
        var debug_alloc = std.testing.FailingAllocator.init(allocator, .{});

        const Map = std.StringHashMap(usize);
        var map = Map.init(debug_alloc.allocator());
        defer map.deinit();
        std.debug.print("\nStringHashMap\n", .{});
        try bench(allocator, &map, Map.put, Map.get, Map.remove);

        std.debug.print("\nStringHashMap allocator stats {s}", .{debug_allocator_stats(debug_alloc)});
    }

    if (build_opts.art) {
        var debug_alloc = std.testing.FailingAllocator.init(allocator, .{});

        const T = Art(usize);
        var t = T.init(debug_alloc.allocator());
        defer t.deinit();
        std.debug.print("\nArt\n", .{});

        try bench(allocator, &t, T.set, T.get, T.delete);

        std.debug.print("\nArt allocator stats {s}\n", .{debug_allocator_stats(debug_alloc)});
        try t.print_pool_stats();
    }
}

fn parseStatus(data: []const u8) []const u8 {
    const ParseState = enum { Empty, FoundStart, FoundEnd };

    var lineBegin = true;
    var state: ParseState = .Empty;
    var start: usize = 0;
    var end: usize = 0;
    for (data, 0..) |c, i| {
        switch (state) {
            .Empty => {
                if (lineBegin) {
                    if (std.mem.eql(u8, data[i .. i + 6], "VmData")) {
                        start = i;
                        state = .FoundStart;
                    }
                    lineBegin = false;
                }
                if (c == '\n') {
                    lineBegin = true;
                }
            },
            .FoundStart => {
                if (lineBegin) {
                    if (std.mem.eql(u8, data[i .. i + 6], "VmSwap")) {
                        state = .FoundEnd;
                    }
                    lineBegin = false;
                }
                if (c == '\n') {
                    lineBegin = true;
                }
            },
            .FoundEnd => {
                if (c == '\n') {
                    std.debug.assert(end == 0);
                    end = i;
                    return data[start..end];
                }
            },
        }
    }
    unreachable;
}
