// TODO we could get nasty with the leaf, the current implementation allocates a new leaf, but..
// TODO instead of allocating a new leaf we could allocate the node+leaf space and on the node is just a bit flag

// TODO when the node have a partial value also always have 1 child. This special case should be take into consideration

// This todo is after the first ones, because is tricky and we may not get performance improvement
// TODO just treat all node on the union as ptrs. the trick is that node3 and partial (node1)
// TODO will be next to the right. This would affect the leaf allocation too.

// TODO i am not demoting the nodes when a delete happens. This can crash the program under certain conditions
// TODO make a test to check this

const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const pool_allocator = @import("pool_allocator.zig");
const Pool = pool_allocator.Pool;

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
        const NodePool = Pool(Node, .{});
        const LeafPool = Pool(Leaf, .{});

        pub const Node = struct {
            /// List of bytes to be used on path compression (Pesimistic approach)
            const Partial = struct {
                const PARTIAL_SIZE = 15;
                items: [PARTIAL_SIZE]u8 = undefined,
                length: u8 = 0,

                node1: Node1 = Node1{},
                const Node1 = struct {
                    label: u8 = 0,
                    ptr: ?*Node = null,
                    pub fn set(self: *Node1, label: u8, child: *Node) void {
                        self.label = label;
                        self.ptr = child;
                    }
                };

                pub fn deinit(self: Partial, allocator: mem.Allocator) void {
                    if (self.node1.ptr) |node| {
                        node.deinit(allocator);
                    }
                }

                fn for_each(self: *const Partial, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                    if (self.node1.ptr) |node| {
                        node.for_each(self.node1.label, level, ctx, fun);
                    }
                }

                pub fn get(self: Partial, label: u8) ?*Node {
                    if (self.node1.label == label) {
                        return self.node1.ptr;
                    }
                    return null;
                }

                pub inline fn is_full(self: Partial) bool {
                    return self.node1.ptr != null;
                }

                pub fn append(self: *Partial, label: u8, child: *Node) void {
                    assert(self.node1.label == 0);
                    self.node1.ptr = child;
                    self.node1.label = label;
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

                /// compares the compressed path of a node with the current key and returns the number of equal bytes
                fn check_prefix(self: Partial, key: []const u8) u8 {
                    var count: u8 = 0;
                    const len = @min(self.length, key.len);
                    for (0..len) |i| {
                        if (self.items[i] != key[i]) {
                            break;
                        }
                        count += 1;
                    }
                    fassert(count <= len, "count {d} len {d}", .{ count, len });
                    return count;
                }

                /// move the data in the prexif n positions so if we move `some_data` 4 positions
                /// would result in `_data`
                fn move_prefix_forwards(self: *Partial, n: u8) void {
                    assert(n <= self.length);
                    const new_length = self.length - n;
                    mem.copyForwards(u8, self.items[0..new_length], self.items[n..self.length]);
                    self.length = new_length;
                }

                test move_prefix_forwards {
                    var partial = Partial{};

                    partial.appendSlice("some_data");
                    partial.move_prefix_forwards(4);
                    try std.testing.expectEqualSlices(u8, "_data", partial.slice());
                }

                fn prefix_len(self: *const Partial) u8 {
                    return self.length;
                }

                fn set_prefix(self: *Partial, prefix: []const u8) void {
                    assert(prefix.len <= Partial.PARTIAL_SIZE);

                    @memcpy(self.items[0..prefix.len], prefix);
                    self.length = @intCast(prefix.len);
                    assert(std.mem.eql(u8, self.slice(), self.items[0..self.length]));
                    assert(std.mem.eql(u8, self.slice(), prefix));
                }
            };

            // partial: Partial = Partial{},
            leaf: ?*Leaf = null,
            node: union(enum) {
                partial: Partial,
                node3: Node3,
                node16: *Node16,
                node48: *Node48,
                node256: *Node256,
            },

            /// creates an empty and ready to use node
            fn new(pool: *NodePool, t: type) *Node {
                const new_node = pool.create() catch unreachable;
                new_node.* = Node{
                    .node = switch (t) {
                        Node3 => .{ .node3 = Node3{} },
                        Partial => .{ .partial = Partial{} },
                        else => {
                            @compileError("expected type " ++
                                @typeName(ARTree.Node3) ++ " or " ++
                                @typeName(ARTree.Node.Partial) ++
                                ", got " ++ @typeName(t));
                        },
                    },
                };
                return new_node;
            }
            /// creates a leaf node with all the necessary intermediate nodes for prefix to fit
            ///
            /// the prefix param is the path to get to the leaf. This is used for path compression.
            ///
            /// parent--[label]-->node_with_compressed_path-->leaf
            ///
            /// node_with_compressed_path can consist of several nodes because the max size of
            /// a single compressed path is `Partial.PARTIAL_SIZE`
            fn new_leaf(node_pool: *NodePool, leaf_pool: *LeafPool, _prefix: []const u8, value: T) *Node {
                var prefix = _prefix;
                var new_node = node_pool.create() catch unreachable;
                new_node.* = Node{
                    .node = .{ .partial = Partial{} },
                    .leaf = Leaf.new(leaf_pool, value),
                };
                while (prefix.len > 0) {
                    // if the prefix is larger than what partial can contain
                    // than we create a partial with the maximum that can contains
                    if (prefix.len > Partial.PARTIAL_SIZE) {
                        const start = prefix.len - Partial.PARTIAL_SIZE;
                        const end = prefix.len;

                        new_node.node.partial.set_prefix(prefix[start..end]);
                        prefix = prefix[0..start];

                        const node = node_pool.create() catch unreachable;
                        node.* = Node{ .node = .{ .partial = Partial{} } };
                        node.append(prefix[prefix.len - 1], new_node);

                        prefix = prefix[0 .. prefix.len - 1];

                        new_node = node;
                    } else {
                        new_node.node.partial.set_prefix(prefix);
                        prefix.len = 0;
                    }
                }
                return new_node;
            }

            fn deinit(self: *Node, allocator: mem.Allocator) void {
                switch (self.node) {
                    .partial => |partial| partial.deinit(allocator),
                    .node3 => self.node.node3.deinit(allocator),
                    .node16 => self.node.node16.deinit(allocator),
                    .node48 => self.node.node48.deinit(allocator),
                    .node256 => self.node.node256.deinit(allocator),
                }

                switch (self.node) {
                    .node16 => |v| allocator.destroy(v),
                    .node48 => |v| allocator.destroy(v),
                    .node256 => |v| allocator.destroy(v),
                    else => {},
                }
            }

            /// free the memory of this node but do not free the childs
            fn destroy(self: *Node, pool: *NodePool, allocator: mem.Allocator) void {
                switch (self.node) {
                    .node16 => |v| allocator.destroy(v),
                    .node48 => |v| allocator.destroy(v),
                    .node256 => |v| allocator.destroy(v),
                    else => {},
                }
                pool.destroy(self);
            }

            pub inline fn prefix_len(self: Node) u8 {
                return switch (self.node) {
                    .partial => |v| v.prefix_len(),
                    else => 0,
                };
            }

            pub inline fn check_prefix(self: Node, key: []const u8) u8 {
                return switch (self.node) {
                    .partial => |v| v.check_prefix(key),
                    else => 0,
                };
            }

            /// appends a new child to the node.
            ///
            /// you should check if the node should be promoted before calling this function
            fn append(self: *Node, label: u8, child: *Node) void {
                switch (self.node) {
                    .partial => self.node.partial.append(label, child),
                    .node3 => self.node.node3.append(label, child),
                    .node16 => self.node.node16.append(label, child),
                    .node48 => self.node.node48.append(label, child),
                    .node256 => self.node.node256.append(label, child),
                }
            }

            /// only promote when the node is full
            fn promote(self: *Node, allocator: mem.Allocator) void {
                switch (self.node) {
                    .partial => |v| {
                        assert(self.childs() == 1);
                        assert(v.node1.ptr != null);
                        var new_node = Node3{};
                        new_node.append(v.node1.label, v.node1.ptr.?);
                        self.node = .{ .node3 = new_node };
                    },
                    .node3 => |v| {
                        assert(v.childs == Node3.NUM);

                        var new_node = allocator.create(Node16) catch unreachable;
                        new_node.* = Node16{};
                        @memcpy(new_node.ptrs[0..Node3.NUM], v.ptrs[0..]);
                        @memcpy(new_node.labels[0..Node3.NUM], v.labels[0..]);
                        new_node.childs = Node3.NUM;
                        self.node = .{ .node16 = new_node };
                    },
                    .node16 => |v| {
                        assert(v.childs == 16);
                        var new_node = allocator.create(Node48) catch unreachable;
                        new_node.* = Node48{};
                        @memcpy(new_node.ptrs[0..16], v.ptrs[0..]);
                        // stores indexes of new_node.ptrs on the new_node.keys
                        for (v.labels, 0..) |key, i| {
                            new_node.idxs[key] = @intCast(i);
                        }
                        new_node.childs = 16;
                        allocator.destroy(v);
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

            inline fn get(self: *const Node, label: u8) ?*Node {
                return switch (self.node) {
                    inline else => |v| v.get(label),
                };
            }

            inline fn for_each(self: *const Node, label: ?u8, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                fun(self, label, level, ctx);
                switch (self.node) {
                    inline else => |v| v.for_each(level + 1, ctx, fun),
                }
            }

            inline fn set(self: *Node, label: u8, child: *Node) void {
                return switch (self.node) {
                    .partial => self.node.partial.node1.set(label, child),
                    .node3 => self.node.node3.set(label, child),
                    .node16 => self.node.node16.set(label, child),
                    .node48 => self.node.node48.set(label, child),
                    .node256 => self.node.node256.set(label, child),
                };
            }

            inline fn childs(self: *const Node) usize {
                return switch (self.node) {
                    .partial => |v| if (v.node1.ptr != null) 1 else 0,
                    inline else => |v| v.childs,
                };
            }

            inline fn is_full(self: *const Node) bool {
                return switch (self.node) {
                    inline else => |v| v.is_full(),
                };
            }

            fn merge_if_possible(self: *Node, pool: *NodePool, allocator: mem.Allocator) void {
                if (self.childs() != 1) {
                    return;
                }
                if (self.leaf != null) {
                    return;
                }
                const child = self.node.partial.node1.ptr.?;
                if (child.childs() != 1) {
                    return;
                }
                // sum of the child label, the child prefix and the current prefix must not be less
                // than `Partial.PARTIAL_SIZE`
                if (child.prefix_len() + 1 + self.prefix_len() > Partial.PARTIAL_SIZE) {
                    return;
                }
                if (self.node == .node3) self.node = .{ .partial = self.node.node3.demote() };
                assert(self.node == .partial);
                self.merge(pool, allocator);
            }

            /// Can merge only if:
            /// - only if self.leaf is null
            /// - only if parent node has 1 child and the child node also have 1 child or 0
            /// the nodes can be merge
            fn merge(self: *Node, pool: *NodePool, allocator: mem.Allocator) void {
                assert(self.childs() == 1);
                assert(self.node == .partial);
                assert(self.leaf == null);
                assert(self.node.partial.node1.ptr != null);

                const child = self.node.partial.node1.ptr.?;
                const label = self.node.partial.node1.label;
                assert(child.childs() == 1);
                assert(child.node == .partial);

                assert((self.prefix_len() + 1 + child.prefix_len()) <= Partial.PARTIAL_SIZE);

                self.node.partial.appendSlice(&[1]u8{label});
                self.node.partial.appendSlice(child.node.partial.slice());
                if (child.leaf) |leaf| {
                    self.leaf = leaf;
                }
                self.node.partial.node1 = child.node.partial.node1;
                child.destroy(pool, allocator);
            }

            fn remove_leaf(self: *Node, node_pool: *NodePool, leaf_pool: *LeafPool, allocator: mem.Allocator) T {
                assert(self.leaf != null);

                const v = self.leaf.?.value;
                leaf_pool.destroy(self.leaf.?);
                self.leaf = null;
                const num_childs = self.childs();
                if (num_childs == 1) {
                    self.merge_if_possible(node_pool, allocator);
                }
                return v;
            }
        };
        const Node3 = struct {
            const NUM = 3;
            /// key and ptr have matching positions
            labels: [NUM]u8 = undefined,
            ptrs: [NUM]?*Node = [1]?*Node{null} ** NUM,
            childs: u8 = 0,

            fn deinit(self: *Node3, allocator: mem.Allocator) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit(allocator);
                }
            }

            fn for_each(self: *const Node3, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.for_each(self.labels[i], level, ctx, fun);
                }
            }

            fn get(self: *const Node3, label: u8) ?*Node {
                for (0..self.childs) |i| {
                    if (label == self.labels[i]) {
                        return self.ptrs[i];
                    }
                }
                return null;
            }

            fn set(self: *Node3, label: u8, child: *Node) void {
                assert(self.get(label) != null);

                for (0..self.childs) |i| {
                    if (label == self.labels[i]) {
                        self.ptrs[i] = child;
                        return;
                    }
                }
                unreachable;
            }

            fn append(self: *Node3, label: u8, child: *Node) void {
                assert(self.childs < NUM);
                assert(self.get(label) == null);

                self.labels[self.childs] = label;
                self.ptrs[self.childs] = child;
                self.childs += 1;
            }

            inline fn is_full(self: *const Node3) bool {
                return self.childs == NUM;
            }

            fn demote(self: Node3) Node.Partial {
                fassert(self.childs == 1, "exepected 1 got {d}", .{self.childs});
                assert(self.ptrs[0] != null);
                return Node.Partial{
                    .node1 = .{
                        .label = self.labels[0],
                        .ptr = self.ptrs[0].?,
                    },
                };
            }
        };
        const Node16 = struct {
            /// key and ptr have matching positions
            labels: [16]u8 = undefined, // use simd comparison to find the key
            ptrs: [16]?*Node = [1]?*Node{null} ** 16,
            childs: u8 = 0,

            fn deinit(self: *Node16, allocator: mem.Allocator) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.deinit(allocator);
                }
            }

            fn for_each(self: *const Node16, level: usize, ctx: anytype, fun: YieldFN(@TypeOf(ctx))) void {
                for (0..self.childs) |i| {
                    self.ptrs[i].?.for_each(self.labels[i], level, ctx, fun);
                }
            }

            fn get(self: *const Node16, label: u8) ?*Node {
                const keyvec: @Vector(16, u8) = self.labels;
                const idx = std.simd.firstIndexOfValue(keyvec, label) orelse return null;
                if (idx >= self.childs) {
                    return null;
                }
                return self.ptrs[idx];
            }

            fn set(self: *Node16, label: u8, child: *Node) void {
                assert(self.get(label) != null);

                const keyvec: @Vector(16, u8) = self.labels;
                const idx = std.simd.firstIndexOfValue(keyvec, label) orelse unreachable;
                assert(idx < self.childs);
                self.ptrs[idx] = child;
            }

            fn append(self: *Node16, label: u8, child: *Node) void {
                assert(self.childs < 16);
                assert(self.get(label) == null);

                self.labels[self.childs] = label;
                self.ptrs[self.childs] = child;
                self.childs += 1;
            }

            inline fn is_full(self: *const Node16) bool {
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

            inline fn is_full(self: *const Node48) bool {
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

            inline fn is_full(self: *const Node256) bool {
                assert(self.childs != 256);
                return false;
            }
        };
        const Leaf = struct {
            /// A tree owned bytes to be used for lazy expansion
            partial: []const u8 = "",
            value: T,

            fn new(pool: *LeafPool, value: T) *Leaf {
                const resp = pool.create() catch unreachable;
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
        node_pool: NodePool,
        leaf_pool: LeafPool,

        pub fn init(allocator: mem.Allocator) ARTree {
            var node_pool = NodePool.init(allocator);
            const leaf_pool = LeafPool.init(allocator);

            var root = Node.new(&node_pool, Node3);
            const node256 = allocator.create(Node256) catch unreachable;
            node256.* = Node256{};
            root.node = .{ .node256 = node256 };
            return ARTree{
                .allocator = allocator,
                .root = root,
                .node_pool = node_pool,
                .leaf_pool = leaf_pool,
            };
        }

        pub fn deinit(self: *ARTree) void {
            self.root.deinit(self.allocator);
            self.node_pool.deinit();
            self.leaf_pool.deinit();
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
                    assert(node.node == .partial);
                    assert(eq < node.prefix_len());
                    const prefix = node.node.partial.items;
                    assert(search[eq] != prefix[eq]);
                    assert(node.node.partial.length > 0);

                    if (eq == search.len) {
                        const label_node = prefix[eq];
                        // the key is a subset of the compressed path
                        const new_node_leaf = Node.new_leaf(&self.node_pool, &self.leaf_pool, search, value);
                        assert(node.node == .partial);

                        node.node.partial.move_prefix_forwards(eq + 1);

                        new_node_leaf.append(label_node, node);

                        // replace node in parent
                        parent.?.set(prev_search.?, new_node_leaf);

                        node.merge_if_possible(&self.node_pool, self.allocator);

                        return null;
                    } else if (eq < search.len and eq != 0) {
                        // the key is not a subset of the compressed path
                        const parent_compressed_path = search[0 .. eq - 1];
                        const leaf_prefix = if (eq + 1 < search.len) search[eq + 1 ..] else &[_]u8{};
                        const label_intermediate = search[eq - 1];
                        const label_leaf = search[eq];
                        const label_node = prefix[eq];

                        const new_node_parent = Node.new(&self.node_pool, Node.Partial);
                        new_node_parent.node.partial.set_prefix(parent_compressed_path);

                        const intermadiate_node = Node.new(&self.node_pool, Node3);
                        const new_leaf_node = Node.new_leaf(&self.node_pool, &self.leaf_pool, leaf_prefix, value);

                        new_node_parent.append(label_intermediate, intermadiate_node);

                        intermadiate_node.append(label_node, node);
                        intermadiate_node.append(label_leaf, new_leaf_node);

                        node.node.partial.move_prefix_forwards(eq + 1);

                        parent.?.set(prev_search.?, new_node_parent);

                        node.merge_if_possible(&self.node_pool, self.allocator);

                        return null;
                    } else if (eq < search.len and eq == 0) {
                        // eq == 0 -> true
                        const leaf_prefix = if (eq + 1 < search.len) search[eq + 1 ..] else &[_]u8{};
                        const label_node = prefix[eq];
                        const label_leaf = search[eq];

                        const new_node_parent = Node.new(&self.node_pool, Node3);
                        const new_leaf_node = Node.new_leaf(&self.node_pool, &self.leaf_pool, leaf_prefix, value);

                        new_node_parent.append(label_node, node);
                        new_node_parent.append(label_leaf, new_leaf_node);

                        node.node.partial.move_prefix_forwards(eq + 1);

                        parent.?.set(prev_search.?, new_node_parent);

                        node.merge_if_possible(&self.node_pool, self.allocator);

                        return null;
                    } else unreachable;
                }
                search = search[eq..];
                if (search.len == 0) {
                    if (node.prefix_len() == eq) {
                        // replace the current leaf as the search and prefix are the same
                        if (node.leaf) |leaf| {
                            response = leaf.value;
                            leaf.value = value;
                        } else {
                            node.leaf = Leaf.new(&self.leaf_pool, value);
                        }
                        return response;
                    } else {
                        // gets here only the search completely to the prefix but the prefix is larger
                        // here we swap the nodes.
                        //
                        // before:
                        // parent node (value1, prefix: "1234")
                        //
                        // new_key: "12", new_value: value2
                        //
                        // after:
                        // parent node (value2, prefix: "12")
                        // edge to the child (label: "3")
                        // child node (value1, prefix: "4")
                        const eql_set = node.node.partial.items[0..eq];
                        const new_parent_node = Node.new_leaf(&self.node_pool, &self.leaf_pool, eql_set, value);
                        if (node.node.partial.items[eq..].len > 0) {
                            // set the current node to be child of the new parent
                            new_parent_node.append(node.node.partial.items[eq + 1], node);
                            if (node.node.partial.items[eq + 1 ..].len > 0) {
                                node.node.partial.move_prefix_forwards(eq + 1);
                            } else {
                                // transform to node3
                                node.promote(self.allocator);
                            }
                        } else {
                            // transform to node3
                            node.promote(self.allocator);
                        }
                        assert(prev_search != null);
                        assert(parent != null);
                        parent.?.set(prev_search.?, new_parent_node);

                        return null;
                    }
                }

                if (node.get(search[0])) |n| {
                    parent = node;
                    prev_search = search[0];
                    node = n;
                    search = search[1..];
                } else {
                    switch (node.node) {
                        .partial => {
                            if (!node.is_full()) {
                                // partial has empty child so we just append the leaf
                                const new_node_leaf = Node.new_leaf(&self.node_pool, &self.leaf_pool, search[1..], value);
                                node.append(search[0], new_node_leaf);
                                return null;
                            }
                            if (node.node.partial.length == 0) {
                                node.promote(self.allocator);
                                const new_node_leaf = Node.new_leaf(&self.node_pool, &self.leaf_pool, search[1..], value);
                                node.append(search[0], new_node_leaf);
                                return null;
                            } else {
                                // partial cannot be appended, but cannot be promoted mindessly
                                // because the partial would be lose

                                // save prefix information
                                const prefix_items = node.node.partial.items;
                                const prefix_length = node.node.partial.length;

                                node.promote(self.allocator);

                                const new_parent_node = Node.new(&self.node_pool, Node.Partial);
                                // set all te complete previous prefix but ommits the last byte
                                new_parent_node.node.partial.set_prefix(prefix_items[0 .. prefix_length - 1]);
                                // swap nodes. `new_parent_node` is the child of the previous parent of `node`
                                // and now `node` is child of `new_parent_node`
                                new_parent_node.append(prefix_items[prefix_length - 1], node);
                                parent.?.set(prev_search.?, new_parent_node);

                                const new_leaf = Node.new_leaf(&self.node_pool, &self.leaf_pool, search[1..], value);
                                node.append(search[0], new_leaf);
                                return null;
                            }
                        },
                        else => {
                            if (node.is_full()) {
                                node.promote(self.allocator);
                            }
                            const new_node_leaf = Node.new_leaf(&self.node_pool, &self.leaf_pool, search[1..], value);
                            node.append(search[0], new_node_leaf);
                            return null;
                        },
                    }
                }
            }
            // create or update leaf TODO implement logic for lazy expansion of leaf
            if (node.leaf) |leaf| {
                response = leaf.value;
                leaf.value = value;
            } else {
                node.leaf = Leaf.new(&self.leaf_pool, value);
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
                        const v = node.remove_leaf(&self.node_pool, &self.leaf_pool, self.allocator);
                        // ?? merge if possible is preform when removing the leaf // parent.?.merge_if_possible(&self.node_pool, self.allocator);
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
                    const v = node.remove_leaf(&self.node_pool, &self.leaf_pool, self.allocator);
                    // parent.?.merge_if_possible(&self.node_pool, self.allocator);
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

const doSearch = struct {
    fn func(line: [:0]const u8, linei: usize, container: anytype, _: anytype, comptime T: type) anyerror!void {
        const v = container.get(line);
        try std.testing.expectEqual(v, valAsType(T, linei));
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
        _ = try eachLineDo(doSearch, lines, &t, null, T);
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
        // std.debug.print("inserting line {d}\n", .{linei});
        try do(line, linei, container, data, T);
        linei += 1;
    }
    return linei - 1;
}

test "promote" {
    const Z = Tree(usize);
    var seed: [32]u8 = undefined;
    try std.posix.getrandom(&seed);
    std.debug.print("seed {d}\n", .{&seed});
    var chacha = std.rand.ChaCha.init(seed);
    const rand = chacha.random();

    var pool = Z.NodePool.init(tal);
    defer pool.deinit();
    var leaf_pool = Z.LeafPool.init(tal);
    defer leaf_pool.deinit();

    var node = Z.Node.new(&pool, Z.Node3);
    defer node.deinit(tal);
    const entry = struct {
        key: u8,
        node: *Z.Node,
    };
    var entries: [256]entry = undefined;
    for (0..256) |i| {
        entries[i].key = @intCast(i);
        entries[i].node = Z.Node.new_leaf(&pool, &leaf_pool, "", rand.int(usize));
    }
    for (entries, 0..) |e, i| {
        if (i == Z.Node3.NUM or i == 16 or i == 48) {
            fassert(node.is_full(), "{d} {d}", .{ i, node.childs() });
        }
        if (node.is_full()) {
            assert(i == Z.Node3.NUM or i == 16 or i == 48);
            switch (i) {
                Z.Node3.NUM => {
                    assert(node.node == .node3);
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

test "new_leaf" {
    {
        var pool = Tree(usize).NodePool.init(tal);
        defer pool.deinit();
        var leaf_pool = Tree(usize).LeafPool.init(tal);
        defer leaf_pool.deinit();
        const n = Tree(usize).Node.new_leaf(&pool, &leaf_pool, "key", 1);
        defer n.deinit(tal);
        try std.testing.expect(n.leaf != null);
        try std.testing.expectEqual(n.leaf.?.value, 1);
    }
    {
        var pool = Tree(usize).NodePool.init(tal);
        defer pool.deinit();
        var leaf_pool = Tree(usize).LeafPool.init(tal);
        defer leaf_pool.deinit();
        const base = [5]u8{ 'l', 'a', 'r', 'g', 'e' };
        const key: []const u8 = &(base ** 4);
        const n = Tree(usize).Node.new_leaf(&pool, &leaf_pool, key, 1);
        defer n.deinit(tal);
        try std.testing.expect(n.leaf == null);
        const child = n.node.partial.node1.ptr.?;
        try std.testing.expect(child.leaf != null);
        try std.testing.expectEqual(child.leaf.?.value, 1);

        try std.testing.expectEqualDeep(child.node.partial.slice(), &(base ** 3));
        try std.testing.expectEqualDeep(n.node.partial.slice(), base[0..4]);
        try std.testing.expectEqual(n.node.partial.node1.label, 'e');
    }
}

test "Node16" {
    const Z = Tree(usize);
    var pool = Z.NodePool.init(std.testing.allocator);
    defer pool.deinit();
    var leaf_pool = Z.LeafPool.init(std.testing.allocator);
    defer leaf_pool.deinit();

    var n = Z.Node16{};
    const a = Z.Node.new_leaf(&pool, &leaf_pool, "", 1);
    defer a.deinit(std.testing.allocator);
    const b = Z.Node.new_leaf(&pool, &leaf_pool, "", 2);
    defer b.deinit(std.testing.allocator);
    const c = Z.Node.new_leaf(&pool, &leaf_pool, "", 3);
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
    const node16 = try std.testing.allocator.create(Z.Node16);
    node16.* = n;

    var node = Z.Node{
        .node = .{ .node16 = node16 },
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

test "size" {
    const s = union(enum) {
        node3: Tree(i32).Node3,
        node16: *Tree(i32).Node16,
        node48: *Tree(i32).Node48,
        node256: *Tree(i32).Node256,
    };

    std.debug.print("node3 size {d} {d}\n", .{ @sizeOf(Tree(i32).Node3), @sizeOf(?*Tree(i32).Node3) });
    std.debug.print("node size {d}\n", .{@sizeOf(Tree(i32).Node)});
    std.debug.print("partial size {d}\n", .{@sizeOf(Tree(i32).Node.Partial)});
    std.debug.print("node union {d}\n", .{@sizeOf(s)});
    std.testing.refAllDecls(Tree(usize).Node3);
}
