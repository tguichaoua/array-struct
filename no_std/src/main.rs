#![no_std]

#[cfg(feature = "std")]
extern crate std;

use array_struct::array_struct;

array_struct! {
    @[
        std( feature = "std" )
    ]

    struct Foo {
        a,
        b,
        c,
    }
}

fn main() {}
