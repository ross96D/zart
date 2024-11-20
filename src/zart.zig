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
            /// List of bytes to be used on path compression (Pesimistic approach)
            const Partial = struct {
                const PARTIAL_SIZE = 15;
                items: [PARTIAL_SIZE]u8 = undefined,
                length: u8 = 0,

                pub fn append(self: *Partial, item: u8) void {
                    assert(self.length < Partial.PARTIAL_SIZE);
                    self.items[self.length] = item;
                    self.length += 1;
                }

                pub fn appendSlice(self: *Partial, items: []const u8) void {
                    fassert(self.length + items.len <= Partial.PARTIAL_SIZE, "length {d} items length {d}", .{ self.length, items.len });
                    for (items, 0..) |item, i| {
                        self.items[self.length + i] = item;
                    }
                    self.length += @intCast(items.len);
                }

                pub inline fn slice(self: Partial) []const u8 {
                    return self.items[0..self.length];
                }
            };

            partial: Partial = Partial{},
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
                    .node = .{ .node4 = Node4{} },
                };
                return new_node;
            }
            /// creates a leaf node with all the necessary intermediate nodes for prefix to fit
            fn new_leaf(allocator: mem.Allocator, _prefix: []const u8, value: T) *Node {
                var prefix = _prefix;
                var new_node = allocator.create(Node) catch unreachable;
                new_node.* = Node{
                    .node = .{ .node4 = Node4{} },
                    .leaf = Leaf.new(allocator, value),
                };
                while (prefix.len > 0) {
                    if (prefix.len > Partial.PARTIAL_SIZE) {
                        const start = prefix.len - Partial.PARTIAL_SIZE;
                        const end = prefix.len;
                        new_node.set_prefix(prefix[start..end]);
                        prefix = prefix[0..start];

                        const node = allocator.create(Node) catch unreachable;
                        node.* = Node{ .node = .{ .node4 = Node4{} } };
                        node.append(prefix[prefix.len - 1], new_node);
                        prefix = prefix[0 .. prefix.len - 1];
                        new_node = node;
                    } else {
                        new_node.set_prefix(prefix);
                        prefix.len = 0;
                    }
                }
                return new_node;
            }

            test new_leaf {
                {
                    const n = Tree(usize).Node.new_leaf(tal, "key", 1);
                    defer n.deinit(tal);
                    try std.testing.expect(n.leaf != null);
                    try std.testing.expectEqual(n.leaf.?.value, 1);
                }
                {
                    const base = [5]u8{ 'l', 'a', 'r', 'g', 'e' };
                    const key: []const u8 = &(base ** 4);
                    const n = Tree(usize).Node.new_leaf(tal, key, 1);
                    defer n.deinit(tal);
                    try std.testing.expect(n.leaf == null);
                    const child = n.node.node4.ptrs[0].?;
                    try std.testing.expect(child.leaf != null);
                    try std.testing.expectEqual(child.leaf.?.value, 1);

                    std.debug.print("\noriginal: {s}\n{s}:{d} - {c} - {s}:{d}\n", .{
                        key,
                        n.partial.slice(),
                        n.partial.slice().len,
                        n.node.node4.keys[0],
                        child.partial.slice(),
                        child.partial.slice().len,
                    });

                    try std.testing.expectEqualDeep(child.partial.slice(), &(base ** 3));
                    try std.testing.expectEqualDeep(n.partial.slice(), base[0..4]);
                    try std.testing.expectEqual(n.node.node4.keys[0], 'e');
                }
            }

            fn deinit(self: *Node, allocator: mem.Allocator) void {
                switch (self.node) {
                    .node4 => self.node.node4.deinit(allocator),
                    .node16 => self.node.node16.deinit(allocator),
                    .node48 => self.node.node48.deinit(allocator),
                    .node256 => self.node.node256.deinit(allocator),
                }
                self.destroy(allocator, true);
            }

            /// free the memory of this node but do not free the childs
            fn destroy(self: *Node, allocator: mem.Allocator, _leaf: bool) void {
                if (self.leaf != null and _leaf) {
                    allocator.destroy(self.leaf.?);
                }
                switch (self.node) {
                    .node48 => |v| allocator.destroy(v),
                    .node256 => |v| allocator.destroy(v),
                    else => {},
                }
                allocator.destroy(self);
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
            fn promote(self: *Node, allocator: mem.Allocator) void {
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
                        var new_node = allocator.create(Node48) catch unreachable;
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
                        var new_node = allocator.create(Node256) catch unreachable;
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
                        allocator.destroy(v);
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
                defer node.deinit(tal);
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
                                node.promote(tal);
                                assert(node.get(e.key) == null);
                                assert(node.node == .node16);
                            },
                            16 => {
                                assert(node.node == .node16);
                                assert(node.get(e.key) == null);
                                node.promote(tal);
                                assert(node.get(e.key) == null);
                                assert(node.node == .node48);
                            },
                            48 => {
                                assert(node.node == .node48);
                                assert(node.get(e.key) == null);
                                node.promote(tal);
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
            fn check_prefix(self: *Node, key: []const u8) u8 {
                var count: u8 = 0;
                const len = @min(self.partial.length, key.len);
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
            fn move_prefix_forwards(self: *Node, n: u8) void {
                assert(n <= self.partial.length);
                const new_length = self.partial.length - n;
                mem.copyForwards(u8, self.partial.items[0..new_length], self.partial.items[n..self.partial.length]);
                self.partial.length = new_length;
            }

            test move_prefix_forwards {
                var node = Node.new(std.testing.allocator);
                defer node.destroy(std.testing.allocator, true);

                node.partial.appendSlice("some_data");
                node.move_prefix_forwards(4);
                try std.testing.expectEqualSlices(u8, "_data", node.partial.slice());
            }

            fn prefix_len(self: *const Node) u8 {
                return self.partial.length;
            }

            fn set_prefix(self: *Node, prefix: []const u8) void {
                assert(prefix.len <= Partial.PARTIAL_SIZE);

                @memcpy(self.partial.items[0..prefix.len], prefix);
                self.partial.length = @intCast(prefix.len);
                assert(std.mem.eql(u8, self.partial.slice(), self.partial.items[0..self.partial.length]));
                assert(std.mem.eql(u8, self.partial.slice(), prefix));
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

            fn merge_if_possible(self: *Node, allocator: mem.Allocator) void {
                if (self.childs() != 1) {
                    return;
                }
                if (self.leaf != null) {
                    return;
                }
                const child = self.node.node4.ptrs[0].?;
                // sum of the child label, the child prefix and the current prefix must not be less
                // than `Partial.PARTIAL_SIZE`
                if (child.prefix_len() + 1 + self.prefix_len() > Partial.PARTIAL_SIZE) {
                    return;
                }
                self.merge(allocator);
            }

            fn merge(self: *Node, allocator: mem.Allocator) void {
                assert(self.childs() == 1);
                assert(self.node == .node4);

                const child = self.node.node4.ptrs[0].?;
                const label = self.node.node4.keys[0];
                assert(child.leaf == null or (child.leaf != null and self.leaf == null));
                assert((self.partial.length + 1 + child.partial.length) <= Partial.PARTIAL_SIZE);

                self.partial.append(label);
                self.partial.appendSlice(child.partial.slice());
                self.node = child.node;
                if (child.leaf) |leaf| {
                    self.leaf = leaf;
                }
                child.destroy(allocator, false);
            }

            fn remove_leaf(self: *Node, allocator: mem.Allocator) T {
                assert(self.leaf != null);

                const v = self.leaf.?.value;
                allocator.destroy(self.leaf.?);
                self.leaf = null;
                const num_childs = self.childs();
                if (num_childs == 1) {
                    self.merge_if_possible(allocator);
                }
                return v;
            }
        };
        const Node4 = struct {
            /// key and ptr have matching positions
            keys: [4]u8 = undefined,
            ptrs: [4]?*Node = [1]?*Node{null} ** 4,
            childs: u8 = 0,

            fn deinit(self: *Node4, allocator: mem.Allocator) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit(allocator);
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

            fn deinit(self: *Node16, allocator: mem.Allocator) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit(allocator);
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
                defer a.deinit(std.testing.allocator);
                const b = Z.Node.new_leaf(std.testing.allocator, "", 2);
                defer b.deinit(std.testing.allocator);
                const c = Z.Node.new_leaf(std.testing.allocator, "", 3);
                defer c.deinit(std.testing.allocator);

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
                };

                const keylist = [16]u8{ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
                for (keylist) |k| {
                    try std.testing.expect(node.get(k) != null);
                }
                node.promote(std.testing.allocator);

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

            fn deinit(self: *Node48, allocator: mem.Allocator) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit(allocator);
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

            fn deinit(self: *Node256, allocator: mem.Allocator) void {
                for (self.idxs) |node| {
                    if (node) |n| {
                        n.deinit(allocator);
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
            self.root.deinit(self.allocator);
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
                    assert(node.partial.length > 0);
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
                        node.promote(self.allocator);
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
                        const v = node.remove_leaf(self.allocator);
                        parent.?.merge_if_possible(self.allocator);
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
                    const v = node.remove_leaf(self.allocator);
                    parent.?.merge_if_possible(self.allocator);
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

test "large_keys" {
    var t = Tree(usize).init(tal);
    defer t.deinit();
    const key = "a_very_large_key___________________________________";
    _ = t.set(key, 1);
    const v = t.get(key) orelse return error.NotFound;
    try std.testing.expectEqual(v, 1);
}

const doInsert = struct {
    fn func(line: [:0]const u8, linei: usize, container: anytype, _: anytype, comptime T: type) anyerror!void {
        _ = container.set(line, valAsType(T, linei));
        // try std.testing.expect(result == .missing);
    }
}.func;

// test "insert many keys" {
//     var lines = try readFileLines(tal, "./testdata/words.txt");
//     defer deinitLines(tal, &lines);
//     inline for (ValueTypes) |T| {
//         var t = Tree(T).init(tal);
//         defer t.deinit();
//         const _lines = try eachLineDo(doInsert, lines, &t, null, T);
//         _ = _lines;
//         // try std.testing.expectEqual(t.size, _lines);
//     }
// }

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
