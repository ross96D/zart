# Summary

This repository is an implementation of the adaptive radix tree described on [The adaptive radix tree: ARTful indexing for main-memory databases](https://ieeexplore.ieee.org/document/6544812). 

# TODO
- [ ] Add fuzzy test
- [ ] Implement a memory pool strategy for fast node allocation/deallocation. Make it configurable at build time.
- [ ] Node size is to large (64 bytes), Node3 take a 32 bytes, have an almost always null pointer to Leaf. Partial is 16 bytes, ideally Node size should be 24 bytes. This reduce memory consumption and may improve speed as more nodes can fit on the cache layer.

# Benchmark results against std.StringHashMap

This adaptative radix implementation copys the part of the key necessary to not depend on the passed key so is not equivalent to StringHashMap on this aspect, which give StringHashMap a huge advantage. But either way this is a perspective on how fast/slow and memory efficient the implementation is.

```
StringHashMap

StringHashMap memory
VmData:    23740 kB
VmStk:       136 kB
VmExe:       216 kB
VmLib:         8 kB
VmPTE:       100 kB
VmSwap:        0 kB

insert 18ms, search 5ms, delete 5ms combined 29ms

StringHashMap allocator stats allocations: 17 deallocations: 16 allocated_bytes: 26214608 freed_bytes: 13107384


Adaptative radix tree

Adaptative radix tree memory
VmData:    75788 kB
VmStk:       136 kB
VmExe:       216 kB
VmLib:         8 kB
VmPTE:       212 kB
VmSwap:        0 kB

insert 41ms, search 18ms, delete 43ms combined 103ms

Adaptative radix tree allocator stats allocations: 573197 deallocations: 259093 allocated_bytes: 29079664 freed_bytes: 7181976
```