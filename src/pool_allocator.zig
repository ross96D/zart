//! The idea is as follow
//!
//! - The pool is for a type X of size S
//! - We keep a list of blocks pointers (maybe a linked list is a better data structure for this)
//! - We keep a sorted list of (block ponter, reusable_elements)
//! - When create is call we check for an item on the free list first (on a oredered manner)
//!     if free list is emtpy
//!         get last block and ask for a new element (create a new block if last block is full)
//!
//! - When delete is call we:
//!     1. search the block and add this block as reusable to the list of
//!     reusable blocks
//!     2. check if that block is completely freed and if so, we free the block and removed from
//!     the list of block pointers
//!
//!

const std = @import("std");
const Allocator = std.mem.Allocator;
const Error = Allocator.Error;
const assert = std.debug.assert;

pub fn Pool(T: type) type {
    const SIZE = @sizeOf(T);
    _ = SIZE;
    const BLOCK_SIZE = 512;
    return struct {
        const Self = @This();

        const Block = struct {
            block: [BLOCK_SIZE]T = undefined,
            length: usize,

            inline fn reset(self: *Block) void {
                self.lenght = 0;
            }

            inline fn is_full(self: *const Block) bool {
                return self.lenght == BLOCK_SIZE;
            }

            inline fn is_empty(self: *const Block) bool {
                // return std.mem.eql(bool, &self.active, &all_inactive);
            }

            fn next(self: *Block) *T {
                assert(!self.is_full());
                // TODO
                const ret = &self.block[self.lenght];
                self.active[self.lenght] = true;
                self.lenght += 1;
                return ret;
            }
        };

        const BlockReusableItems = struct {
            ptr: *Block,
        };

        const FreeList = struct {
            // sorted list of pointrs to block
            blocks: SortedList(usize),
            inline fn init(allocator: Allocator) FreeList {
                return .{ .blocks = SortedList(usize).init(allocator) };
            }
        };

        allocator: Allocator,
        // we should write this our self to tune the grow scale... TODO
        allocated_list: SortedList(*Block),
        // list containing the index of the free items on allocated_list
        free_list: FreeList,

        pub fn init(allocator: Allocator) Self {
            return Self{
                .allocator = allocator,
                .allocated_list = std.ArrayList(Block).init(allocator),
                .free_list = unreachable,
            };
        }

        pub fn create(self: *Self) Error!*T {
            // TODO check free list
            if (self.allocated_list.len() == 0) {
                var block = try self.allocator.create(Block);
                block.reset();
                self.allocated_list.append(block);
                return block.next();
            }
            // for (self.)

            // try self.allocated_list.addOne();
            // last_block = &self.allocated_list.items[self.allocated_list.items.len - 1];
        }

        pub fn destroy(self: *Self, item: *T) Error!void {
            _ = self;
            _ = item;
        }
    };
}

fn SortedList(T: type) type {
    return struct {
        const Self = @This();

        values: std.ArrayList(T),

        pub inline fn init(allocator: Allocator) Self {
            return .{ .values = std.ArrayList(T).init(allocator) };
        }
        pub inline fn deinit(self: Self) void {
            self.values.deinit();
        }

        pub inline fn len(self: Self) usize {
            return self.values.items.len;
        }

        pub inline fn last(self: Self) T {
            return self.values.items[self.len() - 1];
        }

        pub inline fn slice(self: Self) []T {
            return self.values.items;
        }

        pub fn append(self: *Self, v: T) Error!void {
            const pos = self.position(v);
            try self.values.append(v);

            std.mem.copyBackwards(T, self.values.items[pos + 1 ..], self.values.items[pos .. self.len() - 1]);
            self.values.items[pos] = v;
        }

        pub inline fn index_of(self: Self, v: T) ?usize {
            return binary_search(self.values.items, v);
        }

        pub fn remove(self: *Self, v: T) void {
            const index = self.index_of(v) orelse return;

            std.mem.copyForwards(T, self.values.items[index .. self.len() - 1], self.values.items[index + 1 ..]);
            self.values.items.len -= 1;
        }

        fn position(self: Self, v: T) usize {
            if (self.len() == 0) {
                return 0;
            }
            const array = self.values.items;

            var low: usize = 0;
            var high = self.len() - 1;
            var pos: usize = 0;

            while (low < high) {
                pos = @divFloor((high + low + 1), 2);
                if (pos == 0) {
                    break;
                }
                if (v < array[pos]) {
                    high = pos - 1;
                } else if (v > array[pos]) {
                    low = pos;
                } else {
                    return pos;
                }
            }
            pos = low;

            if (v > array[pos]) {
                return pos + 1;
            }
            return pos;
        }

        fn binary_search(array: []T, v: T) ?usize {
            const length = array.len;

            var low: usize = 0;
            var high: usize = length;
            while (low < high) {
                const i: usize = (low + high) / 2;
                if (v > array[i]) {
                    low = i + 1;
                } else if (v < array[i]) {
                    high = i;
                } else {
                    return i;
                }
            }
            return null;
        }
    };
}

test SortedList {
    { // basic
        var list = SortedList(usize).init(std.testing.allocator);
        defer list.deinit();

        try list.append(3);
        try list.append(5);
        try list.append(1);
        const s = list.slice();
        try std.testing.expectEqual(1, s[0]);
        try std.testing.expectEqual(3, s[1]);
        try std.testing.expectEqual(5, s[2]);

        try std.testing.expectEqual(0, list.index_of(1));
        try std.testing.expectEqual(1, list.index_of(3));
        try std.testing.expectEqual(2, list.index_of(5));

        list.remove(3);
        try std.testing.expectEqual(1, s[0]);
        try std.testing.expectEqual(5, s[1]);
        try list.append(3);

        list.remove(1);
        try std.testing.expectEqual(3, s[0]);
        try std.testing.expectEqual(5, s[1]);
        try list.append(1);

        list.remove(5);
        try std.testing.expectEqual(1, s[0]);
        try std.testing.expectEqual(3, s[1]);
        try list.append(5);
    }

    { // fuzzy
        { // append
            var list = SortedList(usize).init(std.testing.allocator);
            defer list.deinit();

            const seed: u64 = 1732828618; // @intCast(std.time.timestamp());
            var rand = std.Random.DefaultPrng.init(seed);
            const random = rand.random();

            for (0..50000) |_| {
                const i = random.intRangeAtMost(usize, 0, 100);
                try list.append(i);
                if (random.boolean()) {
                    list.remove(i);
                }
            }
            var before: usize = 0;
            for (list.values.items, 0..) |item, i| {
                std.testing.expect(item >= before) catch |err| {
                    std.debug.print("seed {d} iter {d} item {d} before {d}\n", .{ seed, i, item, before });
                    std.debug.print("list {d}\n", .{list.values.items});
                    return err;
                };
                before = item;
            }
        }

        { // search
            var list = SortedList(usize).init(std.testing.allocator);
            defer list.deinit();

            const seed: u64 = 1732828618; // @intCast(std.time.timestamp());
            var rand = std.Random.DefaultPrng.init(seed);
            const random = rand.random();

            var elems = std.ArrayList(usize).init(std.testing.allocator);
            defer elems.deinit();

            for (0..50000) |_| {
                const i = random.intRangeAtMost(usize, 0, 100);
                try list.append(i);
                if (random.boolean()) {
                    list.remove(i);
                } else {
                    try elems.append(i);
                }
            }
            var before: usize = 0;
            for (list.values.items, 0..) |item, i| {
                std.testing.expect(item >= before) catch |err| {
                    std.debug.print("seed {d} iter {d} item {d} before {d}\n", .{ seed, i, item, before });
                    std.debug.print("list {d}\n", .{list.values.items});
                    return err;
                };
                before = item;
            }

            for (elems.items) |item| {
                try std.testing.expect(list.index_of(item) != null);
            }
        }
    }
}
