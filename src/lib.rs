/*!
An array-like structures generator.

- Non-procedural macro
- Compatible with `#[no_std]`

See [array_struct] for more details.
*/

/// Same as `array_struct` for `#[no_std]` context.
///
/// This will sets the `std` meta property to `any()` by default
/// which always evaluates to `false`.
#[macro_export]
macro_rules! array_struct_no_std {
    (
        $(
            @[ $($meta_props:tt)* ]
        )?

        $(#[$outer:meta])*
        $vis:vis struct $name:ident {
            $(
                $(#[$field_meta:meta])*
                $field:ident
            ),+
            $(,)?
        }
    ) => {
        $crate::__internal_array_struct!(
            @[
                std(any()) // evaluates to `false`
                $($($meta_props)*)?
            ]

            $(#[$outer])*
            $vis struct $name {
                $(
                    $(#[$field_meta])*
                    $field
                ),+
            }
        );
    };
}

/// Generates a array like struct.
///
/// All of the generated struct's fields having the same type generic type
/// and implements many useful methods.
/// See [this example][crate::Example] for a list of all the methods.
///
/// For example, the following code
///
/// ```
/// # use array_struct::array_struct;
/// array_struct! {
///     pub struct Foo {
///         a, b, c
///     }
/// }
/// ```
///
/// will generates the following struct
///
/// ```
/// pub struct Foo<T> {
///     pub a: T,
///     pub b: T,
///     pub c: T,
/// }
/// ```
///
/// # Meta Properties
///
/// You may pass one or multiple "meta properties" to this macro to control the
/// code that will be generated.
///
/// The meta properties must be passed before the struct definition between `@[` and `]`.
/// Each property takes the form of a function call.
///
///
/// Example:
/// ```
/// # use array_struct::array_struct;
/// array_struct! {
///     @[
///         std( feature = "std" )
///         common_traits( any() )
///     ]
///
///     struct Foo {
///         a, b, c    
///     }
/// }
/// ```
///
/// ## `std`
///
/// Accepts a [`cfg` predicate] which determines whether or not the generated struct may
/// implement features that require the standard library.
///
/// Default to `all()` which evaluates to `true`.
///
/// ## `common_traits`
///
/// Accepts a [`cfg` predicate] which determines whether or not the generated struct
/// should automatically derive the common traits
/// (`Debug`, `Default`, `Clone`, `Copy`, `PartialEq`, `Eq`, `PartialOrd`, `Ord` and `Hash`).
///
/// Default to `all()` which evaluates to `true`.
///
/// ## `try`
///
/// Accepts a [`cfg` predicate] which determines whether or not the generated struct may
/// implement `try_*` methods using the [`Try`][core::ops::Try] trait.
///
/// Default to `any()` which evaluates to `false`.
///
/// ## `try_res`
///
/// Accepts a [`cfg` predicate] which determines whether or not the generated struct may
/// implement `try_*` methods using [`Result`].
///
/// Default to `all()` which evaluates to `true`.
///
/// *Note*: `try` and `try_res` both implement the same methods and are therefore mutually incompatible.
/// Make sure the predicates you use may not be `true` at the same time.
///
/// [`cfg` predicate]: https://doc.rust-lang.org/reference/conditional-compilation.html
#[macro_export]
macro_rules! array_struct {
    (
        $(
            @[ $($meta_props:tt)* ]
        )?

        $(#[$outer:meta])*
        $vis:vis struct $name:ident {
            $(
                $(#[$field_meta:meta])*
                $field:ident
            ),+
            $(,)?
        }
    ) => {
        $crate::__internal_array_struct!(
            @[ $($($meta_props)*)? ]

            $(#[$outer])*
            $vis struct $name {
                $(
                    $(#[$field_meta])*
                    $field
                ),+
            }
        );
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __internal_array_struct {
    (
        $(
            @[ $($meta_props:tt)* ]
        )?

        $(#[$outer:meta])*
        $vis:vis struct $name:ident {
            $(
                $(#[$field_meta:meta])*
                $field:ident
            ),+
            $(,)?
        }
    ) => {
        $crate::__internal_array_struct!(
            @parse_meta_props
            struct: [
                $(#[$outer])*
                $vis struct $name {
                    $(
                        $(#[$field_meta])*
                        $field
                    ),*
                }
            ]

            common_traits: [ all() ]
            use_std: [ all() ]
            try: [ any() ]
            try_res: [ all() ]

            $( $($meta_props)* )?
        );
    };

    /* -------------------------------------------------------------------------- */

    (
        @parse_meta_props
        struct: [ $($struct:tt)* ]

        common_traits: [ $($_:tt)* ]
        use_std: [ $($use_std:tt)* ]
        try: [ $($try:tt)* ]
        try_res: [ $($try_res:tt)* ]

        common_traits($($common_traits:tt)*)

        $($meta_props:tt)*
    ) => {
        $crate::__internal_array_struct!(
            @parse_meta_props
            struct: [ $($struct)* ]

            common_traits: [ $($common_traits)* ]
            use_std: [ $($use_std)* ]
            try: [ $($try)* ]
            try_res: [ $($try_res)* ]

            $($meta_props)*
        );
    };

    (
        @parse_meta_props
        struct: [ $($struct:tt)* ]

        common_traits: [ $($common_traits:tt)* ]
        use_std: [ $($_:tt)* ]
        try: [ $($try:tt)* ]
        try_res: [ $($try_res:tt)* ]

        std( $($use_std:tt)* )

        $($meta_props:tt)*
    ) => {
        $crate::__internal_array_struct!(
            @parse_meta_props
            struct: [ $($struct)* ]

            common_traits: [ $($common_traits)* ]
            use_std: [ $($use_std)* ]
            try: [ $($try)* ]
            try_res: [ $($try_res)* ]

            $($meta_props)*
        );
    };

    (
        @parse_meta_props
        struct: [ $($struct:tt)* ]

        common_traits: [ $($common_traits:tt)* ]
        use_std: [ $($use_std:tt)* ]
        try: [ $($_:tt)* ]
        try_res: [ $($try_res:tt)* ]

        try( $($try:tt)* )

        $($meta_props:tt)*
    ) => {
        $crate::__internal_array_struct!(
            @parse_meta_props
            struct: [ $($struct)* ]

            common_traits: [ $($common_traits)* ]
            use_std: [ $($use_std)* ]
            try: [ $($try)* ]
            try_res: [ $($try_res)* ]

            $($meta_props)*
        );
    };

    (
        @parse_meta_props
        struct: [ $($struct:tt)* ]

        common_traits: [ $($common_traits:tt)* ]
        use_std: [ $($use_std:tt)* ]
        try: [ $($try:tt)* ]
        try_res: [ $($_:tt)* ]

        try_res( $($try_res:tt)* )

        $($meta_props:tt)*
    ) => {
        $crate::__internal_array_struct!(
            @parse_meta_props
            struct: [ $($struct)* ]

            common_traits: [ $($common_traits)* ]
            use_std: [ $($use_std)* ]
            try: [ $($try)* ]
            try_res: [ $($try_res)* ]

            $($meta_props)*
        );
    };

    (
        @parse_meta_props
        struct: [ $($struct:tt)* ]

        common_traits: [ $($common_traits:tt)* ]
        use_std: [ $($use_std:tt)* ]
        try: [ $($try:tt)* ]
        try_res: [ $($try_res:tt)* ]

        /* no more meta_props to parse */
    ) => {
        $crate::__internal_array_struct!(
            @impl

            common_traits: [ $($common_traits)* ]
            use_std: [ $($use_std)* ]
            try: [ $($try)* ]
            try_res: [ $($try_res)* ]

            $($struct)*
        );
    };

    /* -------------------------------------------------------------------------- */

    (
        @impl

        common_traits: [ $($common_traits:tt)* ]
        use_std: [ $($use_std:tt)* ]
        try: [ $($try:tt)* ]
        try_res: [ $($try_res:tt)* ]

        $(#[$outer:meta])*
        $vis:vis struct $name:ident {
            $(
                $(#[$field_meta:meta])*
                $field:ident
            ),+
            $(,)?
        }
    ) => {
        #[cfg_attr(
            $($common_traits)*,
            derive(
                ::core::fmt::Debug,
                ::core::default::Default,
                ::core::clone::Clone,
                ::core::marker::Copy,
                ::core::cmp::PartialEq,
                ::core::cmp::Eq,
                ::core::cmp::PartialOrd,
                ::core::cmp::Ord,
                ::core::hash::Hash,
            )
        )]
        $(#[$outer])*
        $vis struct $name<T> {
            $(
                $(#[$field_meta])*
                pub $field: T,
            )*
        }

        #[cfg( $($use_std)* )]
        $crate::__internal_array_struct!(@impl_std struct $name { $($field),* } );

        const _: () = {
            use ::core::ops::FnMut;
            use ::core::convert::From;
            use ::core::option::Option;

            impl<T> $name<T> {
                /// The number of items [`
                #[doc=stringify!($name)]
                /// `] has.
                pub const LEN: usize = $crate::__internal_array_struct!(@ident_count $($field)*);

                /// The [`
                #[doc=stringify!($name)]
                /// `]'s field names.
                pub const NAMES: [&'static str; <$name<()>>::LEN] = [ $( stringify!($field) ),* ];

                /// The number of items [`
                #[doc=stringify!($name)]
                /// `] has.
                #[allow(clippy::len_without_is_empty)]
                #[inline(always)]
                pub const fn len(&self) -> usize {
                    Self::LEN
                }

                /// Initializes [`
                #[doc=stringify!($name)]
                /// `], where each field's value is the returned value from `f`.
                ///
                /// `f` receives the field's name and index.
                #[inline(always)]
                pub fn from_fn(mut f: impl FnMut(&'static str, usize) -> T) -> Self {
                    $crate::__internal_array_struct!(@from_fn_impl (f) $($field)*)
                }

                /// Converts from
                #[doc=concat!("`&", stringify!($name), "<T>`")]
                /// to
                #[doc=concat!("`", stringify!($name), "<&T>`.")]
                #[inline(always)]
                pub const fn as_ref(&self) -> $name<&T> {
                    $name {
                        $(
                            $field: &self.$field
                        ),*
                    }
                }

                /// Converts from
                #[doc=concat!("`&mut ", stringify!($name), "<T>`")]
                /// to
                #[doc=concat!("`", stringify!($name), "<&mut T>`.")]
                #[inline(always)]
                pub const fn as_mut(&mut self) -> $name<&mut T> {
                    $name {
                        $(
                            $field: &mut self.$field
                        ),*
                    }
                }

                /// Turns each field into a tuple of this field's name and value.
                #[inline(always)]
                pub fn with_names(self) -> $name<(&'static str, T)>
                {
                    $name {
                        $(
                            $field: (stringify!($field), self.$field)
                        ),*
                    }
                }

                /// Transforms each field using `f`.
                #[inline(always)]
                pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> $name<U> {
                    $name {
                        $(
                            $field: f(self.$field),
                        )*
                    }
                }

                /// Calls the function `f` on each field in order.
                #[inline(always)]
                pub fn for_each(self, mut f: impl FnMut(T)) {
                    $(
                        f(self.$field);
                    )*
                }

                /// Folds every element into an accumulator by applying an operation, returning the final result.
                ///
                /// `fold()` takes two arguments: an initial value, and a closure with two arguments: an ‘accumulator’,
                /// and an element. The closure returns the value that the accumulator should have for the next iteration.
                ///
                /// The initial value is the value the accumulator will have on the first call.
                ///
                /// After applying this closure to every element, `fold()` returns the accumulator.
                #[inline(always)]
                pub fn fold<B>(self, init: B, mut f: impl FnMut(B, T) -> B) -> B {
                    let mut state = init;

                    $(
                        state = f(state, self.$field);
                    )*

                    state
                }

                $crate::__internal_array_struct!(@reduce $($field)*);

                /// Zips together the fields of two [`
                #[doc=stringify!($name)]
                /// `].
                #[inline(always)]
                pub fn zip<U>(self, other: $name<U>) -> $name<(T, U)> {
                    $name {
                        $(
                            $field: (self.$field, other.$field)
                        ),*
                    }
                }

                /// Tests if every element matches a predicate.
                ///
                /// `all()` takes a closure that returns `true` or `false`.
                /// It applies this closure to each element, and if they all return `true`,
                /// then so does `all()`. If any of them return `false`, it returns `false`.
                ///
                /// `all()` is short-circuiting; in other words, it will stop processing as
                /// soon as it finds a `false`, given that no matter what else happens,
                /// the result will also be `false`.
                #[inline(always)]
                pub fn all(self, mut f: impl FnMut(T) -> bool) -> bool {
                    $(
                        f(self.$field)
                    )&&*
                }

                /// Tests if any element matches a predicate.
                ///
                /// `any()` takes a closure that returns `true` or `false`.
                /// It applies this closure to each element, and if any of them return `true`,
                /// then so does `any()`. If they all return `false`, it returns `false`.
                ///
                /// `any()` is short-circuiting; in other words, it will stop processing as
                /// soon as it finds a `true`, given that no matter what else happens,
                /// the result will also be `true`.
                #[inline(always)]
                pub fn any(self, mut f: impl FnMut(T) -> bool) -> bool {
                    $(
                        f(self.$field)
                    )||*
                }
            }

            impl<T, U> $name<(T, U)> {
                /// Unzips each field's tuple into two [`
                #[doc = stringify!($name)]
                /// `].
                #[inline(always)]
                pub fn unzip(self) -> ($name<T>, $name<U>) {
                    (
                        $name {
                            $(
                                $field: self.$field.0,
                            )*
                        },
                        $name {
                            $(
                                $field: self.$field.1,
                            )*
                        }
                    )
                }
            }

            impl<T> $name<Option<T>> {
                /// Transposes a
                #[doc=concat!("`", stringify!($name), "<Option<T>>`")]
                /// into a
                #[doc=concat!("`Option<", stringify!($name), "<T>>`.")]
                #[inline(always)]
                pub fn transpose(self) -> Option<$name<T>> {
                    Option::Some(
                        $name {
                            $(
                                $field: self.$field?,
                            )*
                        }
                    )
                }
            }


            impl<T> From<[T; <$name<()>>::LEN]> for $name<T> {
                #[inline(always)]
                fn from(array: [T; <$name<()>>::LEN]) -> Self {
                    let [ $($field),* ] = array;
                    Self { $($field),* }
                }
            }

            impl<T> From<$name<T>> for [T; <$name<()>>::LEN] {
                #[inline(always)]
                fn from(this: $name<T>) -> Self {
                    [ $( this.$field ),* ]
                }
            }

        };

        #[cfg( $($try)* )]
        $crate::__internal_array_struct!(@impl_try struct $name { $($field),* } );

        #[cfg( $($try_res)* )]
        $crate::__internal_array_struct!(@impl_try_res struct $name { $($field),* } );
    };

    /* -------------------------------------------------------------------------- */

    (@ident_count $($x:ident)* ) => {
        $crate::__internal_array_struct!(@ident_count [0] $($x)*)
    };

    (@ident_count [$n:expr] $_:ident $($rest:ident)* ) => {
        $crate::__internal_array_struct!(@ident_count [$n + 1] $($rest)*)
    };

    (@ident_count [$n:expr] ) => {
        $n
    };

    /* -------------------------------------------------------------------------- */

    (@from_fn_impl ($f:expr) $($field:ident)* ) => {
        $crate::__internal_array_struct!(@from_fn_impl ($f) [] [0] $($field)*)
    };

    (@from_fn_impl ($f:expr) [$($tt:tt)*] [$n:expr] $field:ident $($rest:ident)* ) => {
        $crate::__internal_array_struct!(
            @from_fn_impl
            ($f)
            [
                $($tt)*
                $field: $f(stringify!($field), $n),
            ]
            [$n + 1]
            $($rest)*
        )
    };

    (@from_fn_impl ($f:expr) [$($tt:tt)*] [$n:expr] ) => {
        Self {
            $($tt)*
        }
    };

    /* -------------------------------------------------------------------------- */

    (@reduce $first:ident $($field:ident)* ) => {
        /// Reduces the elements to a single one, by repeatedly applying a reducing operation.
        ///
        /// The reducing function is a closure with two arguments: an ‘accumulator’, and an element.
        /// This is the same as [`fold()`][Self::fold] with the first element of the iterator as the initial
        /// accumulator value, folding every subsequent element into it.
        #[inline(always)]
        pub fn reduce(self, mut f: impl ::core::ops::FnMut(T, T) -> T) -> T {
            let mut state = self.$first;

            $(
                state = f(state, self.$field);
            )*

            state
        }
    };

    /* -------------------------------------------------------------------------- */

    (
        @impl_std

        struct $name:ident {
            $( $field:ident ),*
        }
    ) => {
        #[doc(hidden)]
        const _: () =
        {
            use ::core::convert::{From, TryFrom};
            use ::core::result::Result;

            use ::std::vec::Vec;
            use ::std::boxed::Box;
            use ::std::vec;

            impl<T> From<$name<T>> for Vec<T> {
                #[inline(always)]
                fn from(this: $name<T>) -> Vec<T> {
                    let $name { $($field),* } = this;
                    vec! [ $($field),* ]
                }
            }

            impl<T> TryFrom<Vec<T>> for $name<T> {
                type Error = Vec<T>;

                /// Gets the entire contents of the `Vec<T>` as a [`
                #[doc = stringify!($name)]
                /// `], if its size exactly matches [`
                #[doc = concat! (stringify!($name), "::LEN")]
                /// `].
                fn try_from(vec: Vec<T>) -> Result<Self, Self::Error> {
                    let array: [T; <$name<()>>::LEN] = vec.try_into()?;
                    Ok(Self::from(array))
                }
            }

            impl<T> From<$name<T>> for Box<[T]> {
                #[inline(always)]
                fn from(this: $name<T>) -> Box<[T]> {
                    let vec = Vec::from(this);
                    vec.into_boxed_slice()
                }
            }

            impl<T> TryFrom<Box<[T]>> for $name<T> {
                type Error = Box<[T]>;

                /// Gets the entire contents of the `Box<[T]>` as a [`
                #[doc = stringify!($name)]
                /// `], if its size exactly matches [`
                #[doc = concat! (stringify!($name), "::LEN")]
                /// `].
                fn try_from(slice: Box<[T]>) -> Result<Self, Self::Error> {
                    let array: Box<[T; <$name<()>>::LEN]> = slice.try_into()?;
                    Ok(Self::from(array))
                }
            }

            impl<T> From<Box<[T; <$name<()>>::LEN]>> for $name<T> {
                /// Gets the entire contents of the `Box<[T; N]>` as a [`
                #[doc = stringify!($name)]
                /// `].
                #[inline(always)]
                fn from(array: Box<[T; <$name<()>>::LEN]>) -> Self {
                    Self::from(*array)
                }
            }
        };
    };

    /* -------------------------------------------------------------------------- */

    (
        @impl_try

        struct $name:ident {
            $( $field:ident ),*
        }
    ) => {
        #[doc(hidden)]
        const _: () =
        {
            use core::ops::{Try, Residual, FnMut};

            type ChangeOutputType<T: Try<Residual: Residual<V>>, V> = <T::Residual as Residual<V>>::TryType;

            impl<T> $name<T> {
                $crate::__internal_array_struct!(@try_from_fn $($field)*);

                /// A fallible alternative to [`map()`][Self::map].
                #[inline(always)]
                pub fn try_map<R>(self, mut f: impl FnMut(T) -> R)
                -> ChangeOutputType<R, $name<R::Output>>
                where
                    R: Try<Residual: Residual<$name<R::Output>>>
                {
                    ChangeOutputType::<R, $name<R::Output>>::from_output(
                        $name {
                            $(
                                $field: f(self.$field)?,
                            )*
                        }
                    )
                }

                /// A fallible alternative to [`for_each()`][Self::for_each].
                #[inline(always)]
                pub fn try_for_each<R>(self, mut f: impl FnMut(T) -> R) -> R
                where
                    R: Try<Output = ()>
                {
                    $(
                        f(self.$field)?;
                    )*

                    R::from_output(())
                }

                /// A fallible alternative to [`fold()`][Self::fold].
                #[inline(always)]
                pub fn try_fold<B, R>(self, init: B, mut f: impl ::core::ops::FnMut(B, T) -> R) -> R
                where
                    R: Try<Output = B>
                 {
                    let mut state = init;

                    $(
                        state = f(state, self.$field)?;
                    )*

                    R::from_output(state)
                }

                $crate::__internal_array_struct!(@try_reduce $($field)*);
            }
        };
    };

    (@try_from_fn $($field:ident)* ) => {
        /// A fallible alternative to [`from_fn()`][Self::from_fn].
        #[inline(always)]
        pub fn try_from_fn<R>(mut f: impl ::core::ops::FnMut(&'static str, usize) -> R)
            -> <R::Residual as core::ops::Residual<Self>>::TryType
        where
            R: ::core::ops::Try<Output = T>,
            R::Residual: core::ops::Residual<Self>,
        {
            $crate::__internal_array_struct!(@try_from_fn (f) [] [0] $($field)*)
        }
    };

    (@try_from_fn ($f:expr) [$($tt:tt)*] [$n:expr] $field:ident $($rest:ident)* ) => {
        $crate::__internal_array_struct!(
            @try_from_fn
            ($f)
            [
                $($tt)*
                $field: $f(stringify!($field), $n)?,
            ]
            [$n + 1]
            $($rest)*
        )
    };

    (@try_from_fn ($f:expr) [$($tt:tt)*] [$n:expr] ) => {
        <R::Residual as core::ops::Residual<Self>>::TryType::from_output(
            Self {
                $($tt)*
            }
        )
    };

    (@try_reduce $first:ident $($field:ident)* ) => {
        /// A fallible alternative to [`reduce()`][Self::reduce].
        #[inline(always)]
        pub fn try_reduce<R>(self, mut f: impl ::core::ops::FnMut(T, T) -> R) -> R
        where
            R: ::core::ops::Try<Output = T>
        {
            let mut state = self.$first;

            $(
                state = f(state, self.$field)?;
            )*

            R::from_output(state)
        }
    };

     /* -------------------------------------------------------------------------- */

     (
        @impl_try_res

        struct $name:ident {
            $( $field:ident ),*
        }
    ) => {
        #[doc(hidden)]
        const _: () =
        {
            use core::result::Result;
            use core::ops::FnMut;

            impl<T> $name<T> {
                $crate::__internal_array_struct!(@try_res_from_fn $($field)*);

                /// A fallible alternative to [`map()`][Self::map].
                #[inline(always)]
                pub fn try_map<U, E>(self, mut f: impl FnMut(T) -> Result<U, E>) -> Result<$name<U>, E>
                {
                    Result::Ok(
                        $name {
                            $(
                                $field: f(self.$field)?,
                            )*
                        }
                    )
                }

                /// A fallible alternative to [`for_each()`][Self::for_each].
                #[inline(always)]
                pub fn try_for_each<E>(self, mut f: impl FnMut(T) -> Result<(), E>) -> Result<(), E>
                {
                    $(
                        f(self.$field)?;
                    )*

                    Result::Ok(())
                }

                /// A fallible alternative to [`fold()`][Self::fold].
                #[inline(always)]
                pub fn try_fold<B, E>(self, init: B, mut f: impl ::core::ops::FnMut(B, T) -> Result<B, E>) -> Result<B, E> {
                    let mut state = init;

                    $(
                        state = f(state, self.$field)?;
                    )*

                    Result::Ok(state)
                }

                $crate::__internal_array_struct!(@try_res_reduce $($field)*);
            }
        };
    };

    (@try_res_from_fn $($field:ident)* ) => {
        /// A fallible alternative to [`from_fn()`][Self::from_fn].
        #[inline(always)]
        pub fn try_from_fn<E>(mut f: impl ::core::ops::FnMut(&'static str, usize) -> ::core::result::Result<T, E>)
            -> ::core::result::Result<Self, E>
        {
            $crate::__internal_array_struct!(@try_res_from_fn (f) [] [0] $($field)*)
        }
    };

    (@try_res_from_fn ($f:expr) [$($tt:tt)*] [$n:expr] $field:ident $($rest:ident)* ) => {
        $crate::__internal_array_struct!(
            @try_res_from_fn
            ($f)
            [
                $($tt)*
                $field: $f(stringify!($field), $n)?,
            ]
            [$n + 1]
            $($rest)*
        )
    };

    (@try_res_from_fn ($f:expr) [$($tt:tt)*] [$n:expr] ) => {
        ::core::result::Result::Ok(Self {
            $($tt)*
        })
    };

    (@try_res_reduce $first:ident $($field:ident)* ) => {
        /// A fallible alternative to [`reduce()`][Self::reduce].
        #[inline(always)]
        pub fn try_reduce<E>(self, mut f: impl ::core::ops::FnMut(T, T) -> ::core::result::Result<T, E>) -> ::core::result::Result<T, E> {
            let mut state = self.$first;

            $(
                state = f(state, self.$field)?;
            )*

            ::core::result::Result::Ok(state)
        }
    };

    /* -------------------------------------------------------------------------- */

}

/* -------------------------------------------------------------------------- */

#[cfg(doc)]
array_struct! {
    /// An example of generated struct.
    ///
    /// ⚠️ This struct isn't part of this crate, it's only generated when building the docs.
    ///
    /// This struct is generated with the following code:
    /// ```
    /// # use array_struct::array_struct;
    /// array_struct! {
    ///     pub struct Example {
    ///         /// This is the `a` field.
    ///         a,
    ///         /// This is the `b` field.
    ///         b,
    ///         /// This is the `c` field.
    ///         c
    ///     }
    /// }
    /// ```
    pub struct Example {
        /// This is the `a` field.
        a,
        /// This is the `b` field.
        b,
        /// This is the `c` field.
        c
    }
}
