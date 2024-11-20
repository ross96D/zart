const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
fn _fassert(ok: bool, comptime fmt: []const u8, args: anytype) void {
    if (!ok) {
        std.debug.print(fmt ++ "\n", args);
        unreachable;
    }
}
fn _no_assert(_: bool) void {}
fn _no_fassert(_: bool, comptime _: []const u8, _: anytype) void {}
const fassert: fn (bool, comptime []const u8, anytype) void = if (builtin.mode == .ReleaseFast) _no_fassert else _fassert;
const assert: fn (bool) void = if (builtin.mode == .ReleaseFast) _no_assert else std.debug.assert;

pub fn Tree(comptime T: type) type {
    return struct {
        const ARTree = @This();

        pub const Node = struct {
            /// A growable list of bytes to be used on path compression (Pesimistic approach)
            partial: std.ArrayList(u8),
            leaf: ?*Leaf = null,
            node: union(enum) {
                node4: Node4,
                node16: Node16,
                node48: *Node48,
                node256: *Node256,
            },

            /// creates an empty and ready to use node
            fn new(allocator: mem.Allocator) *Node {
                const new_node = allocator.create(Node) catch unreachable;
                new_node.* = Node{
                    .partial = std.ArrayList(u8).init(allocator),
                    .node = .{ .node4 = Node4{} },
                };
                return new_node;
            }
            /// creates an empty and ready to use node with a leaf
            fn new_leaf(allocator: mem.Allocator, prefix: []const u8, value: T) *Node {
                const new_node = allocator.create(Node) catch unreachable;
                new_node.* = Node{
                    .partial = std.ArrayList(u8).init(allocator),
                    .node = .{ .node4 = Node4{} },
                    .leaf = Leaf.new(allocator, value),
                };
                new_node.set_prefix(prefix);
                return new_node;
            }

            fn deinit(self: *Node) void {
                switch (self.node) {
                    .node4 => self.node.node4.deinit(),
                    .node16 => self.node.node16.deinit(),
                    .node48 => self.node.node48.deinit(),
                    .node256 => self.node.node256.deinit(),
                }
                self.destroy(true);
            }

            /// free the memory of this node but do not free the childs
            fn destroy(self: *Node, _leaf: bool) void {
                self.partial.deinit();
                if (self.leaf != null and _leaf) {
                    self.partial.allocator.destroy(self.leaf.?);
                }
                switch (self.node) {
                    .node48 => |v| self.partial.allocator.destroy(v),
                    .node256 => |v| self.partial.allocator.destroy(v),
                    else => {},
                }
                self.partial.allocator.destroy(self);
            }

            /// appends a new child to the node.
            ///
            /// you should check if the node should be promoted before calling this function
            fn append(self: *Node, label: u8, child: *Node) void {
                switch (self.node) {
                    .node4 => self.node.node4.append(label, child),
                    .node16 => self.node.node16.append(label, child),
                    .node48 => self.node.node48.append(label, child),
                    .node256 => self.node.node256.append(label, child),
                }
            }

            /// only promote when the node is full
            fn promote(self: *Node) void {
                switch (self.node) {
                    .node4 => |v| {
                        assert(v.childs == 4);

                        var new_node = Node16{};
                        @memcpy(new_node.ptrs[0..4], v.ptrs[0..]);
                        @memcpy(new_node.keys[0..4], v.keys[0..]);
                        new_node.childs = 4;
                        self.node = .{ .node16 = new_node };
                    },
                    .node16 => |v| {
                        assert(v.childs == 16);
                        var new_node = self.partial.allocator.create(Node48) catch unreachable;
                        new_node.* = Node48{};
                        @memcpy(new_node.ptrs[0..16], v.ptrs[0..]);
                        // stores indexes of new_node.ptrs on the new_node.keys
                        for (v.keys, 0..) |key, i| {
                            new_node.idxs[key] = @intCast(i);
                        }
                        new_node.childs = 16;
                        self.node = .{ .node48 = new_node };
                    },
                    .node48 => |v| {
                        assert(v.childs == 48);
                        var new_node = self.partial.allocator.create(Node256) catch unreachable;
                        new_node.* = Node256{};
                        var count: usize = 0;
                        for (v.idxs, 0..) |idx, i| {
                            if (i == 48) {
                                break;
                            }
                            if (v.ptrs[idx]) |n| {
                                new_node.idxs[i] = n;
                                count += 1;
                            }
                        }
                        assert(count == 48);
                        new_node.childs = 48;
                        self.partial.allocator.destroy(v);
                        self.node = .{ .node256 = new_node };
                    },
                    .node256 => unreachable,
                }
            }

            test promote {
                const Z = Tree(usize);
                var seed: [32]u8 = undefined;
                try std.posix.getrandom(&seed);
                std.debug.print("seed {d}\n", .{&seed});
                var chacha = std.rand.ChaCha.init(seed);
                const rand = chacha.random();

                var node = Z.Node.new(tal);
                defer node.deinit();
                const entry = struct {
                    key: u8,
                    node: *Z.Node,
                };
                var entries: [256]entry = undefined;
                for (0..256) |i| {
                    entries[i].key = @intCast(i);
                    entries[i].node = Z.Node.new_leaf(tal, "", rand.int(usize));
                }
                for (entries, 0..) |e, i| {
                    if (i == 4 or i == 16 or i == 48) {
                        assert(node.is_full());
                    }
                    if (node.is_full()) {
                        assert(i == 4 or i == 16 or i == 48);
                        switch (i) {
                            4 => {
                                assert(node.node == .node4);
                                assert(node.get(e.key) == null);
                                node.promote();
                                assert(node.get(e.key) == null);
                                assert(node.node == .node16);
                            },
                            16 => {
                                assert(node.node == .node16);
                                assert(node.get(e.key) == null);
                                node.promote();
                                assert(node.get(e.key) == null);
                                assert(node.node == .node48);
                            },
                            48 => {
                                assert(node.node == .node48);
                                assert(node.get(e.key) == null);
                                node.promote();
                                assert(node.get(e.key) == null);
                                assert(node.node == .node256);
                            },
                            else => unreachable,
                        }
                    }
                    node.append(e.key, e.node);
                }
                for (entries) |e| {
                    try std.testing.expect(node.get(e.key) != null);
                }
            }

            /// compares the compressed path of a node with the current key and returns the number of equal bytes
            fn check_prefix(self: *Node, key: []const u8) usize {
                var count: usize = 0;
                const len = @min(self.partial.items.len, key.len);
                for (0..len) |i| {
                    if (self.partial.items[i] != key[i]) {
                        break;
                    }
                    count += 1;
                }
                fassert(count <= len, "count {d} len {d}", .{ count, len });
                return count;
            }

            /// move the data in the prexif n positions so if we move `some_data` 4 positions
            /// would result in `_data`
            fn move_prefix_forwards(self: *Node, n: usize) void {
                const new_length = self.partial.items.len - n;
                mem.copyForwards(u8, self.partial.items[0..new_length], self.partial.items[n..]);
                self.partial.items.len = new_length;
            }

            test move_prefix_forwards {
                var node = Node.new(std.testing.allocator);
                defer node.destroy(true);

                try node.partial.appendSlice("some_data");
                node.move_prefix_forwards(4);
                try std.testing.expectEqualSlices(u8, "_data", node.partial.items);
            }

            fn prefix_len(self: *const Node) usize {
                return self.partial.items.len;
            }

            fn set_prefix(self: *Node, prefix: []const u8) void {
                assert(self.partial.items.len == 0);
                self.partial.appendSlice(prefix) catch unreachable;
            }

            fn get(self: *const Node, label: u8) ?*Node {
                return switch (self.node) {
                    inline else => |v| v.get(label),
                };
            }

            fn for_each(self: *const Node, label: ?u8, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                fun(self, label, level, ctx);
                switch (self.node) {
                    inline else => |v| v.for_each(level + 1, ctx, fun),
                }
            }

            fn set(self: *Node, label: u8, child: *Node) void {
                return switch (self.node) {
                    .node4 => self.node.node4.set(label, child),
                    .node16 => self.node.node16.set(label, child),
                    .node48 => self.node.node48.set(label, child),
                    .node256 => self.node.node256.set(label, child),
                };
            }

            fn childs(self: *const Node) usize {
                return switch (self.node) {
                    inline else => |v| v.childs,
                };
            }

            fn is_full(self: *const Node) bool {
                return switch (self.node) {
                    inline else => |v| v.is_full(),
                };
            }

            fn merge_if_possible(self: *Node) void {
                if (self.childs() != 1) {
                    return;
                }
                if (self.leaf != null) {
                    return;
                }
                self.merge();
            }

            fn merge(self: *Node) void {
                assert(self.childs() == 1);
                assert(self.node == .node4);

                const child = self.node.node4.ptrs[0].?;
                const label = self.node.node4.keys[0];
                assert(child.leaf == null or (child.leaf != null and self.leaf == null));

                self.partial.append(label) catch unreachable;
                self.partial.appendSlice(child.partial.items) catch unreachable;
                self.node = child.node;
                if (child.leaf) |leaf| {
                    self.leaf = leaf;
                }
                child.destroy(false);
            }

            fn remove_leaf(self: *Node) T {
                assert(self.leaf != null);

                const v = self.leaf.?.value;
                self.partial.allocator.destroy(self.leaf.?);
                self.leaf = null;
                const num_childs = self.childs();
                if (num_childs == 1) {
                    self.merge();
                }
                return v;
            }
        };
        const Node4 = struct {
            /// key and ptr have matching positions
            keys: [4]u8 = undefined,
            ptrs: [4]?*Node = [1]?*Node{null} ** 4,
            childs: u8 = 0,

            fn deinit(self: *Node4) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit();
                }
            }

            fn for_each(self: *const Node4, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.for_each(self.keys[i], level, ctx, fun);
                }
            }

            fn get(self: *const Node4, label: u8) ?*Node {
                for (0..self.childs) |i| {
                    if (label == self.keys[i]) {
                        return self.ptrs[i];
                    }
                }
                return null;
            }

            fn set(self: *Node4, label: u8, child: *Node) void {
                assert(self.get(label) != null);

                for (0..self.childs) |i| {
                    if (label == self.keys[i]) {
                        self.ptrs[i] = child;
                        return;
                    }
                }
                unreachable;
            }

            fn append(self: *Node4, label: u8, child: *Node) void {
                assert(self.childs < 4);
                assert(self.get(label) == null);

                self.keys[self.childs] = label;
                self.ptrs[self.childs] = child;
                self.childs += 1;
            }

            fn is_full(self: *const Node4) bool {
                return self.childs == 4;
            }
        };
        const Node16 = struct {
            /// key and ptr have matching positions
            keys: [16]u8 = undefined, // use simd comparison to find the key
            ptrs: [16]?*Node = [1]?*Node{null} ** 16,
            childs: u8 = 0,

            fn deinit(self: *Node16) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit();
                }
            }

            fn for_each(self: *const Node16, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.for_each(self.keys[i], level, ctx, fun);
                }
            }

            fn get(self: *const Node16, label: u8) ?*Node {
                const keyvec: @Vector(16, u8) = self.keys;
                const idx = std.simd.firstIndexOfValue(keyvec, label) orelse return null;
                if (idx >= self.childs) {
                    return null;
                }
                return self.ptrs[idx];
            }

            test Node16 {
                const Z = Tree(usize);
                var n = Z.Node16{};
                const a = Z.Node.new_leaf(std.testing.allocator, "", 1);
                defer a.deinit();
                const b = Z.Node.new_leaf(std.testing.allocator, "", 2);
                defer b.deinit();
                const c = Z.Node.new_leaf(std.testing.allocator, "", 3);
                defer c.deinit();

                n.append('a', a);
                n.append('b', b);
                n.append('c', c);
                try std.testing.expectEqual(a, n.get('a'));
                try std.testing.expectEqual(b, n.get('b'));
                try std.testing.expectEqual(c, n.get('c'));
                try std.testing.expectEqual(null, n.get('d'));

                n.append('d', c);
                n.append('e', c);
                n.append('f', c);
                n.append('0', c);
                n.append('1', c);
                n.append('2', c);
                n.append('3', c);
                n.append('4', c);
                n.append('5', c);
                n.append('6', c);
                n.append('7', c);
                n.append('8', c);
                n.append('9', c);

                var node = Z.Node{
                    .node = .{ .node16 = n },
                    .partial = std.ArrayList(u8).init(std.testing.allocator),
                };

                const keylist = [16]u8{ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
                for (keylist) |k| {
                    try std.testing.expect(node.get(k) != null);
                }
                node.promote();

                for (keylist) |k| {
                    try std.testing.expect(node.get(k) != null);
                }
                std.testing.allocator.destroy(node.node.node48);
            }

            fn set(self: *Node16, label: u8, child: *Node) void {
                assert(self.get(label) != null);

                const keyvec: @Vector(16, u8) = self.keys;
                const idx = std.simd.firstIndexOfValue(keyvec, label) orelse unreachable;
                assert(idx < self.childs);
                self.ptrs[idx] = child;
            }

            fn append(self: *Node16, label: u8, child: *Node) void {
                assert(self.childs < 16);
                assert(self.get(label) == null);

                self.keys[self.childs] = label;
                self.ptrs[self.childs] = child;
                self.childs += 1;
            }

            fn is_full(self: *const Node16) bool {
                return self.childs == 16;
            }
        };
        const Node48 = struct {
            /// 256-element array indexed with key bytes directly and store the idx of the ptr array
            idxs: [256]u8 = [1]u8{255} ** 256,
            ptrs: [48]?*Node = [1]?*Node{null} ** 48,
            childs: u8 = 0,

            fn deinit(self: *Node48) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit();
                }
            }

            fn for_each(self: *const Node48, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                for (0..self.childs) |i| {
                    const label = std.mem.indexOf(u8, &self.idxs, &[_]u8{@intCast(i)});
                    self.ptrs[i].?.for_each(@intCast(label.?), level, ctx, fun);
                }
            }

            fn get(self: *const Node48, label: u8) ?*Node {
                const i = self.idxs[label];
                if (i == 255) return null;
                return self.ptrs[i];
            }

            fn set(self: *Node48, label: u8, child: *Node) void {
                assert(self.get(label) != null);

                const i = self.idxs[label];
                self.ptrs[i] = child;
            }

            fn append(self: *Node48, label: u8, child: *Node) void {
                assert(self.childs < 48);
                fassert(self.get(label) == null, "label {d} childs {d}", .{ label, self.childs });

                self.ptrs[self.childs] = child;
                self.idxs[label] = self.childs;
                self.childs += 1;
            }

            fn is_full(self: *const Node48) bool {
                return self.childs == 48;
            }
        };
        const Node256 = struct {
            idxs: [256]?*Node = [1]?*Node{null} ** 256,
            childs: u8 = 0,

            fn deinit(self: *Node256) void {
                for (self.idxs) |node| {
                    if (node) |n| {
                        n.deinit();
                    }
                }
            }

            fn for_each(self: *const Node256, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                for (self.idxs, 0..) |node, i| {
                    if (node) |n| {
                        n.for_each(@intCast(i), level, ctx, fun);
                    }
                }
            }

            fn get(self: *const Node256, label: u8) ?*Node {
                return self.idxs[label];
            }

            fn set(self: *Node256, label: u8, child: *Node) void {
                assert(self.get(label) != null);

                self.idxs[label] = child;
            }

            fn append(self: *Node256, label: u8, child: *Node) void {
                assert(self.childs < 256);
                assert(self.get(label) == null);

                self.idxs[label] = child;
            }

            fn is_full(self: *const Node256) bool {
                assert(self.childs != 256);
                return false;
            }
        };
        const Leaf = struct {
            /// A tree owned bytes to be used for lazy expansion
            partial: []const u8 = "",
            value: T,

            fn new(allocator: mem.Allocator, value: T) *Leaf {
                const resp = allocator.create(Leaf) catch unreachable;
                resp.partial = "";
                resp.value = value;
                return resp;
            }

            /// compares key with partial
            fn eql(self: *Leaf, key: []const u8) bool {
                return std.mem.eql(u8, self.partial, key);
            }
        };

        root: *Node,
        allocator: mem.Allocator,

        pub fn init(allocator: mem.Allocator) ARTree {
            var root = Node.new(allocator);
            const node256 = allocator.create(Node256) catch unreachable;
            node256.* = Node256{};
            root.node = .{ .node256 = node256 };
            return ARTree{
                .allocator = allocator,
                .root = root,
            };
        }

        pub fn deinit(self: ARTree) void {
            self.root.deinit();
        }

        pub fn get(self: *ARTree, key: []const u8) ?T {
            var search = key;
            var node = self.root;

            while (search.len != 0) {
                if (node.leaf) |leaf| {
                    if (leaf.eql(search)) {
                        return leaf.value;
                    }
                }
                const eq = node.check_prefix(search);
                if (eq != node.prefix_len()) {
                    return null;
                }
                search = search[eq..];
                if (search.len == 0) {
                    continue;
                }
                node = node.get(search[0]) orelse return null;
                search = search[1..];
            }
            if (node.leaf) |leaf| {
                if (leaf.eql(search)) {
                    return leaf.value;
                }
            }
            return null;
        }

        pub fn set(self: *ARTree, key: []const u8, value: T) ?T {
            var search = key;
            var node = self.root;
            var parent: ?*Node = null;
            var prev_search: ?u8 = null;
            var response: ?T = null;

            while (search.len != 0) {
                const eq = node.check_prefix(search);
                if (eq != node.prefix_len()) {
                    assert(search[eq] != node.partial.items[eq]);
                    assert(node.partial.items.len > 0);
                    assert(eq < node.prefix_len());

                    const label_leaf = search[eq];
                    const node_leaf = node.partial.items[eq];

                    const new_node_leaf = Node.new_leaf(self.allocator, search[eq + 1 ..], value);

                    var new_node = Node.new(self.allocator);
                    new_node.set_prefix(search[0..eq]);
                    node.move_prefix_forwards(eq + 1);

                    new_node.append(label_leaf, new_node_leaf);
                    new_node.append(node_leaf, node);

                    // replace node in parent
                    parent.?.set(prev_search.?, new_node);
                    return null;
                }
                search = search[eq..];
                if (search.len == 0) {
                    // create or update leaf TODO implement logic for lazy expansion of leaf
                    if (node.leaf) |leaf| {
                        response = leaf.value;
                        leaf.value = value;
                    } else {
                        node.leaf = Leaf.new(self.allocator, value);
                    }
                    return response;
                }

                if (node.get(search[0])) |n| {
                    parent = node;
                    prev_search = search[0];
                    node = n;
                    search = search[1..];
                } else {
                    if (node.is_full()) {
                        node.promote();
                    }
                    const new_node_leaf = Node.new_leaf(self.allocator, search[1..], value);
                    node.append(search[0], new_node_leaf);
                    return null;
                }
            }
            // create or update leaf TODO implement logic for lazy expansion of leaf
            if (node.leaf) |leaf| {
                response = leaf.value;
                leaf.value = value;
            } else {
                node.leaf = Leaf.new(self.allocator, value);
            }
            return response;
        }

        pub fn delete(self: *ARTree, key: []const u8) ?T {
            var search = key;
            var node = self.root;
            var parent: ?*Node = null;
            var prev_search: ?u8 = null;

            while (search.len != 0) {
                if (node.leaf) |leaf| {
                    if (leaf.eql(search)) {
                        const v = node.remove_leaf();
                        parent.?.merge_if_possible();
                        return v;
                    }
                }
                const eq = node.check_prefix(search);
                if (eq != node.prefix_len()) {
                    return null;
                }
                search = search[eq..];
                if (search.len == 0) {
                    continue;
                }
                if (node.get(search[0])) |n| {
                    parent = node;
                    prev_search = search[0];
                    node = n;
                    search = search[1..];
                } else {
                    return null;
                }
            }
            if (node.leaf) |leaf| {
                if (leaf.eql(search)) {
                    const v = node.remove_leaf();
                    parent.?.merge_if_possible();
                    return v;
                }
            }
            return null;
        }

        pub const Entry = struct {
            key: []const u8,
            value: T,
        };

        pub fn YieldFN(CtxT: type) type {
            return fn (node: *const Node, label: ?u8, level: usize, context: CtxT) void;
        }

        pub fn for_each(self: *const ARTree, context: anytype, fun: YieldFN(@TypeOf(context))) void {
            self.root.for_each(null, 0, context, fun);
        }
    };
}

// set to test against many value types (increases test run time)
const test_all_ValueTypes = false;
const ValueTypes = if (test_all_ValueTypes) [_]type{ u8, u16, u32, u64, usize, f32, f64, bool, [24]u8, [3]usize } else [_]type{usize};
fn valAsType(comptime T: type, i: usize) T {
    return switch (@typeInfo(T)) {
        .Int => @truncate(i),
        .Bool => i != 0,
        .Float => @floatFromInt(i),
        .Array => blk: {
            const v: T = undefined;
            for (v) |*it| {
                it.* = @truncate(i);
            }
            break :blk v;
        },

        else => @compileLog(T, @typeInfo(T)),
    };
}

test "call" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Tree(i32).Node);
    std.testing.refAllDecls(Tree(i32).Node16);
}

const tal = std.testing.allocator;
test "basic" {
    {
        var ts = Tree([]const u8).init(tal);
        defer ts.deinit();
        _ = ts.set("key1", "value1");
        _ = ts.set("key2", "value2");
        _ = ts.set("key11", "value3");

        // const Tprint = struct {
        //     fn f(node: *const Tree([]const u8).Node, label: ?u8, _: void) void {
        //         if (label) |l| {
        //             std.debug.print("{c}", .{l});
        //         }
        //         if (node.prefix_len() > 0) {
        //             std.debug.print("{s}", .{node.partial.items});
        //         }
        //         if (node.childs() == 0) {
        //             std.debug.print("\n", .{});
        //         }
        //         // if (node.leaf) |l| {
        //         //     std.debug.print("\nleaf {s}\n", .{l.value});
        //         // }
        //     }
        // }.f;
        // std.debug.print("printing \n", .{});
        // ts.for_each({}, Tprint);
        // std.debug.print("end printing\n", .{});

        const rs1 = ts.get("key1") orelse return error.NotFound;
        const rs2 = ts.get("key2") orelse return error.NotFound;
        const rs3 = ts.get("key11") orelse return error.NotFound;
        try std.testing.expectEqualSlices(u8, rs1, "value1");
        try std.testing.expectEqualSlices(u8, rs2, "value2");
        try std.testing.expectEqualSlices(u8, rs3, "value3");

        try std.testing.expect(ts.delete("key11") != null);
        try std.testing.expect(ts.delete("key2") != null);

        try std.testing.expectEqual(null, ts.get("key11"));
        try std.testing.expectEqual(null, ts.get("key2"));
        try std.testing.expectEqual(ts.get("key1"), "value1");
    }
    {
        var ts = Tree(usize).init(tal);
        defer ts.deinit();
        _ = ts.set("key1", 1);
        _ = ts.set("key2", 2);
        _ = ts.set("key3", 3);

        const rs1 = ts.get("key1") orelse return error.NotFound;
        const rs2 = ts.get("key2") orelse return error.NotFound;
        const rs3 = ts.get("key3") orelse return error.NotFound;
        try std.testing.expectEqual(rs1, 1);
        try std.testing.expectEqual(rs2, 2);
        try std.testing.expectEqual(rs3, 3);
    }

    inline for (ValueTypes) |T| {
        var t = Tree(T).init(tal);
        defer t.deinit();
        const words = [_][:0]const u8{
            "Aaron",
            "Aaronic",
            "Aaronical",
            "123\x0023",
            "123\x003",
        };
        for (words, 0..) |w, i| {
            const v = t.set(w, valAsType(T, i));
            try std.testing.expect(v == null);
        }

        const r0 = t.get("Aaronic");
        try std.testing.expectEqual(valAsType(T, 1), r0);

        const r = t.get("123\x0023");
        // try std.testing.expect(r != null);
        try std.testing.expectEqual(valAsType(T, 3), r);
        const r2 = t.get("123\x003");
        try std.testing.expect(r2 != null);
        try std.testing.expect(r2 == valAsType(T, 4));
        try std.testing.expect(r2 != r);
    }
}

const doInsert = struct {
    fn func(line: [:0]const u8, linei: usize, container: anytype, _: anytype, comptime T: type) anyerror!void {
        _ = container.set(line, valAsType(T, linei));
        // try std.testing.expect(result == .missing);
    }
}.func;

test "insert many keys" {
    var lines = try readFileLines(tal, "./testdata/words.txt");
    defer deinitLines(tal, &lines);
    inline for (ValueTypes) |T| {
        var t = Tree(T).init(tal);
        defer t.deinit();
        const _lines = try eachLineDo(doInsert, lines, &t, null, T);
        _ = _lines;
        // try std.testing.expectEqual(t.size, _lines);
    }
}

pub fn readFileLines(allocator: mem.Allocator, filename: []const u8) !std.ArrayList([:0]const u8) {
    var lines = std.ArrayList([:0]const u8).init(allocator);
    const f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer f.close();
    const reader = f.reader();
    var buf: [512]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try lines.append(try allocator.dupeZ(u8, line));
    }
    return lines;
}

pub fn deinitLines(allocator: mem.Allocator, lines: *std.ArrayList([:0]const u8)) void {
    for (lines.items) |line| {
        allocator.free(line);
    }
    lines.deinit();
}

pub fn eachLineDo(
    do: *const fn (
        line: [:0]const u8,
        linei: usize,
        container: anytype,
        data: anytype,
        comptime T: type,
    ) anyerror!void,
    lines: std.ArrayList([:0]const u8),
    container: anytype,
    data: anytype,
    comptime T: type,
) !usize {
    var linei: usize = 1;
    for (lines.items) |line| {
        try do(line, linei, container, data, T);
        linei += 1;
    }
    return linei - 1;
}
