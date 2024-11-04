# Summary

This repository is an implementation of the adaptive radix tree described on [The adaptive radix tree: ARTful indexing for main-memory databases](https://ieeexplore.ieee.org/document/6544812).

This implementation is slower than https://github.com/travisstaloch/art.zig but it does not push the key ownership to the user, so the user can deallocate the key, the tree would work just fine and achive better space eficiency.
