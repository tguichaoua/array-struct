# Array-Struct

[![github](https://img.shields.io/badge/github-tguichaoua/array--struct-8da0cb?logo=github)](https://github.com/tguichaoua/array-struct)
[![Latest version](https://img.shields.io/crates/v/array-struct.svg)](https://crates.io/crates/array-struct)
[![docs.rs](https://img.shields.io/docsrs/array-struct)](https://docs.rs/array-struct)
[![MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/tguichaoua/array-struct/blob/main/LICENSE-MIT)
[![Apache](https://img.shields.io/badge/license-Apache-blue.svg)](https://github.com/tguichaoua/array-struct/blob/main/LICENSE-APACHE)

---

An array-like structures generator.

- Non-procedural macro
- Compatible with `#[no_std]`

Creates array-like structures with many useful methods such as `map`, `for_each`, etc. (check out the doc for more details).

```rs
use array_struct::array_struct;

array_struct!{
    pub struct Foo {
        a, b, c
    }
}

// Will generates:

pub struct Foo<T> {
    a: T,
    b: T,
    c: T,
}

impl<T> Foo<T> {
    /* Many useful methods */
}
```

Compatible with `#![no_std]` crates that may use `std` with a feature flag.

```rs
#![no_std]

#[cfg(feature = "std")]
extern crate std;

use array_struct::array_struct;

array_struct!{
    @[
        std( feature = "std" )
    ]

    pub struct Foo {
        a, b, c
    }
}
```
