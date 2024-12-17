const std = @import("std");

pub fn stats(allocator: std.mem.Allocator) []const u8 {
    const f = std.fs.openFileAbsolute("/proc/self/status", .{}) catch unreachable;
    const data = f.readToEndAlloc(allocator, 1 << 31) catch unreachable;
    defer allocator.free(data);
    const ret = parseStatus(data);
    return allocator.dupe(u8, ret) catch unreachable;
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
