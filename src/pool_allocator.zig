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
    const ITEM_SIZE = @sizeOf(T);
    const BLOCK_SIZE = 10000;
    return struct {
        const Self = @This();

        const Block = struct {
            fn order(b1: *Block, b2: *Block) std.math.Order {
                const ptr1 = @intFromPtr(b1);
                const ptr2 = @intFromPtr(b2);
                return std.math.order(ptr1, ptr2);
            }

            values: [BLOCK_SIZE]T = undefined,
            lenght: usize,

            inline fn reset(self: *Block) void {
                self.lenght = 0;
            }

            inline fn is_full(self: *const Block) bool {
                return self.lenght == BLOCK_SIZE;
            }

            fn next(self: *Block) *T {
                assert(!self.is_full());
                const ret = &self.values[self.lenght];
                self.lenght += 1;
                return ret;
            }
        };

        const BlockReusableItems = struct {
            fn order(b1: BlockReusableItems, b2: BlockReusableItems) std.math.Order {
                const ptr1 = @intFromPtr(b1.ptr);
                const ptr2 = @intFromPtr(b2.ptr);
                return std.math.order(ptr1, ptr2);
            }

            /// pointer to the allocated Block
            ptr: *Block,
            /// List of indexes that had been freed
            ///
            /// This could be improved if we take into account that the
            /// list max len can only be BLOCK_SIZE
            reusable: std.ArrayList(usize),

            inline fn init(allocator: Allocator, block: *Block) BlockReusableItems {
                return .{
                    .reusable = std.ArrayList(usize).init(allocator),
                    .ptr = block,
                };
            }

            inline fn deinit(self: BlockReusableItems) void {
                self.reusable.deinit();
            }

            fn reuse(self: *BlockReusableItems, allocated_list: *const AllocatedList) ?*T {
                if (self.reusable.items.len == 0) {
                    return null;
                }
                const index_item = self.reusable.pop();
                const index_block = allocated_list.index_of(self.ptr).?;
                const block = allocated_list.slice()[index_block];
                return &block.values[index_item];
            }

            inline fn can_be_free(self: *BlockReusableItems) bool {
                return self.ptr.lenght == self.reusable.items.len;
            }
        };

        const ReusableList = struct {
            const List = SortedList(BlockReusableItems, BlockReusableItems.order);

            /// track all the allocated blocks and the reusable elements
            blocks: List,

            inline fn init(allocator: Allocator) ReusableList {
                return .{
                    .blocks = List.init(allocator),
                };
            }

            inline fn deinit(self: ReusableList) void {
                for (self.blocks.values.items) |b| {
                    b.deinit();
                }
                self.blocks.deinit();
            }

            fn reuse(self: *ReusableList, allocated_list: *const AllocatedList) ?*T {
                for (self.blocks.slice()) |*block| {
                    if (block.reuse(allocated_list)) |elem| {
                        return elem;
                    }
                }
                return null;
            }

            inline fn get(self: ReusableList, block: *Block) ?*BlockReusableItems {
                const index = self.blocks.index_of(.{
                    .ptr = block,
                    .reusable = undefined,
                }) orelse return null;

                return &self.blocks.values.items[index];
            }
        };

        const AllocatedList = SortedList(*Block, Block.order);

        allocator: Allocator,
        // we should write this our self to tune the grow scale... TODO
        allocated_list: AllocatedList,
        // list containing the index of the free items on allocated_list
        reusable_list: ReusableList,

        /// Cache the last allocated for fast create
        last_allocated: *Block = undefined,

        pub inline fn init(allocator: Allocator) Self {
            return Self{
                .allocator = allocator,
                .allocated_list = AllocatedList.init(allocator),
                .reusable_list = ReusableList.init(allocator),
            };
        }

        pub inline fn deinit(self: Self) void {
            self.reusable_list.deinit();
            for (self.allocated_list.slice()) |b| {
                self.allocator.destroy(b);
            }
            self.allocated_list.deinit();
        }

        fn new_block(self: *Self) Error!*Block {
            var block = try self.allocator.create(Block);
            block.reset();
            self.last_allocated = block;
            try self.allocated_list.append(block);
            try self.reusable_list.blocks.append(BlockReusableItems.init(self.allocator, block));
            return block;
        }

        pub fn create(self: *Self) Error!*T {
            if (self.allocated_list.len() == 0) {
                var block = try self.new_block();
                return block.next();
            }
            if (self.reusable_list.reuse(&self.allocated_list)) |elem| {
                return elem;
            }
            if (!self.last_allocated.is_full()) {
                return self.last_allocated.next();
            }
            var block = try self.new_block();
            return block.next();
        }

        pub fn destroy(self: *Self, item: *T) void {
            for (self.allocated_list.slice()) |block| {
                const ptr = @intFromPtr(item);
                const start = @intFromPtr(block);
                const size = BLOCK_SIZE * ITEM_SIZE;
                // this should always be valid because the list is sorted by ptrs on ascending order
                assert(ptr >= start);

                const offset = ptr - start;
                if (offset >= size) {
                    continue;
                }
                const index = @divExact(offset, ITEM_SIZE);
                var reusable_block = self.reusable_list.get(block).?;
                reusable_block.reusable.append(index) catch unreachable;

                if (reusable_block.can_be_free()) {
                    reusable_block.deinit();
                    self.reusable_list.blocks.remove(reusable_block.*);
                    self.allocated_list.remove(block);

                    if (block == self.last_allocated and self.allocated_list.len() != 0) {
                        // we only set a valid element to avoid an invalid address access
                        // but the element as long as is valid is not important
                        // because every block on the list is full and will trigger
                        // the creation of a new block
                        self.last_allocated = self.allocated_list.values.items[0];
                    }

                    self.allocator.destroy(block);
                }
                break;
            }
        }
    };
}

fn SortedList(T: type, compare: fn (T, T) std.math.Order) type {
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
                switch (compare(v, array[pos])) {
                    .eq => return pos,
                    .lt => high = pos - 1,
                    .gt => low = pos,
                }
            }
            pos = low;

            if (compare(v, array[pos]) == .gt) {
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
                switch (compare(v, array[i])) {
                    .eq => return i,
                    .lt => high = i,
                    .gt => low = i + 1,
                }
            }
            return null;
        }
    };
}

test "bench" {
    const MyStruct = struct {
        num: usize,
        some: struct {
            num1: usize,
            num2: usize,
        },
    };
    const LOOPS = 1000000;
    var timer = try std.time.Timer.start();
    {
        timer.reset();
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();

        var p = Pool(MyStruct).init(gpa.allocator());
        defer p.deinit();

        var elems = std.ArrayList(*MyStruct).init(std.testing.allocator);
        defer elems.deinit();
        for (0..LOOPS) |_| {
            const s = try p.create();
            s.num = 1;
            s.some.num1 = 2;
            s.some.num2 = 3;
            try elems.append(s);
        }

        for (elems.items) |e| {
            try std.testing.expectEqual(1, e.num);
            try std.testing.expectEqual(2, e.some.num1);
            try std.testing.expectEqual(3, e.some.num2);
            p.destroy(e);
        }
        const t = timer.read();
        std.debug.print("Pool {}ms\n", .{t / 1000000});
    }

    {
        timer.reset();

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();
        const allocator = gpa.allocator();

        var elems = std.ArrayList(*MyStruct).init(std.testing.allocator);
        defer elems.deinit();
        for (0..LOOPS) |_| {
            const s = try allocator.create(MyStruct);
            s.num = 1;
            s.some.num1 = 2;
            s.some.num2 = 3;
            try elems.append(s);
        }

        for (elems.items) |e| {
            try std.testing.expectEqual(1, e.num);
            try std.testing.expectEqual(2, e.some.num1);
            try std.testing.expectEqual(3, e.some.num2);
            allocator.destroy(e);
        }
        const t = timer.read();
        std.debug.print("GPA {}ms\n", .{t / 1000000});
    }
}

test Pool {
    const MyStruct = struct {
        num: usize,
        some: struct {
            num1: usize,
            num2: usize,
        },
    };
    var p = Pool(MyStruct).init(std.testing.allocator);
    defer p.deinit();

    var elems = std.ArrayList(*MyStruct).init(std.testing.allocator);
    defer elems.deinit();
    for (0..15000) |_| {
        const s = try p.create();
        s.num = 1;
        s.some.num1 = 2;
        s.some.num2 = 3;
        try elems.append(s);
    }

    for (elems.items) |e| {
        try std.testing.expectEqual(1, e.num);
        try std.testing.expectEqual(2, e.some.num1);
        try std.testing.expectEqual(3, e.some.num2);
        p.destroy(e);
    }
}

const ImplOrder = struct {
    fn order_u32(lhs: u32, rhs: u32) std.math.Order {
        return std.math.order(lhs, rhs);
    }
    fn order_usize(lhs: usize, rhs: usize) std.math.Order {
        return std.math.order(lhs, rhs);
    }
    fn order_i32(lhs: i32, rhs: i32) std.math.Order {
        return std.math.order(lhs, rhs);
    }
};

test SortedList {
    {
        var list = SortedList(*Pool(usize).Block, Pool(usize).Block.order).init(std.testing.allocator);
        const ptr1: *Pool(usize).Block = @ptrFromInt(0x7f9cf6f53fe8);
        const ptr2: *Pool(usize).Block = @ptrFromInt(0x7f9cf6f65000);
        defer list.deinit();
        try list.append(ptr1);
        try list.append(ptr2);
        list.remove(ptr1);
        try std.testing.expectEqual(ptr2, list.values.items[0]);
    }

    { // basic
        var list = SortedList(usize, ImplOrder.order_usize).init(std.testing.allocator);
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
            var list = SortedList(usize, ImplOrder.order_usize).init(std.testing.allocator);
            defer list.deinit();

            const seed: u64 = 1732828618; // @intCast(std.time.timestamp());
            var rand = std.Random.DefaultPrng.init(seed);
            const random = rand.random();

            for (0..5000) |_| {
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
            var list = SortedList(usize, ImplOrder.order_usize).init(std.testing.allocator);
            defer list.deinit();

            const seed: u64 = 1732828618; // @intCast(std.time.timestamp());
            var rand = std.Random.DefaultPrng.init(seed);
            const random = rand.random();

            var elems = std.ArrayList(usize).init(std.testing.allocator);
            defer elems.deinit();

            for (0..5000) |_| {
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
