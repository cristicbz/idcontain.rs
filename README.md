idcontain
---

A Rust library for containers built around 'generational id-s': tagged id-s
which prevent errors caused by id reuse, id mixing (between different
containers) and enable 'dangling id' detection (using an id after it was
deleted).

Currently the only implemented type is `IdVec` which is an unordered collection,
sometimes called a `Slab` allocator. It differs from @carllerche's excellent
[Slab](https://crates.io/crates/slab) in its support for the properties
described above at a small cost in memory and speed.

The particular implementation used for preventing ID reuse and mixing has some
caveats (i.e. pathological cases causing false positives in membership tests)
which mean you should not rely on it for memory safety or security. These cases
are quite extreme so you're definitely OK to use this for video games etc. See
the documentation for more details.

Getting Started
---
Add the dependency to your `Cargo.toml` manifest.
```toml

[dependencies]
idcontain = "0.1"
```

Example
---
```rust
extern crate idcontain;

use idcontain::{IdVec, Id};

fn main() {
  let mut id_vec_int1 = IdVec::new();
  let mut id_vec_int2 = IdVec::with_capacity(3);
  let mut id_vec_str = IdVec::with_capacity(1);

  // Inserting an element returns its `Id`.
  let id1: Id<i32> = id_vec_int1.insert(1);
  assert_eq!(id_vec_int1[id1], 1);                // Panicking lookup.
  assert_eq!(id_vec_int1.get(id1), Some(&1));     // Non-panicking lookup.
  id_vec_int1[id1] = 10;                          // In-place mutation.
  assert_eq!(id_vec_int1.remove(id1), Some(10));  // Removal.

  // Id-s are not reused.
  let id2 = id_vec_int1.insert(20);
  assert!(id2 != id1);

  // Id-s from different `IdVec`-s do not collide.
  let id3 = id_vec_int2.insert(20);
  assert!(id3 != id2);
  assert!(id3 != id1);

  // Id-s from `IdVec`-s of different types cannot mixed with compile-time
  // checks.
  let str_id: Id<&'static str> = id_vec_str.insert("hello");
  assert_eq!(id_vec_str[str_id], "hello")

  // Compile-time errors:
  //   str_id == id1
  //   id_vec_int1.get(str_id)
  //   id_vec_str[id1]
}
```

Iteration and other stuff is also supported, check out the documentation!
