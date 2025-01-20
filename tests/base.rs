use array_struct::array_struct;

array_struct! {
    pub struct Foo {
        a, b, c,
    }
}

array_struct! {
    pub struct ComplexNames {
        daipzjdpazjd_damkzjda,
        zaidazpdjazp
    }
}

#[test]
fn len() {
    assert_eq!(Foo::<()>::LEN, 3);
    assert_eq!(Foo::<bool>::LEN, 3);
    assert_eq!(Foo::<u32>::LEN, 3);
    assert_eq!(Foo::<Foo<u8>>::LEN, 3);
    assert_eq!(Foo::<()>::default().len(), 3);

    assert_eq!(ComplexNames::<()>::LEN, 2);
    assert_eq!(ComplexNames::<bool>::LEN, 2);
    assert_eq!(ComplexNames::<u32>::LEN, 2);
    assert_eq!(ComplexNames::<Foo<u8>>::LEN, 2);
    assert_eq!(ComplexNames::<()>::default().len(), 2);
}

#[test]
fn names() {
    assert_eq!(Foo::<()>::NAMES, ["a", "b", "c"]);
    assert_eq!(
        ComplexNames::<()>::NAMES,
        ["daipzjdpazjd_damkzjda", "zaidazpdjazp"]
    );
}

#[test]
fn all() {
    let foo = Foo::from_fn(|_, i| i);
    assert_eq!(foo.a, 0);
    assert_eq!(foo.b, 1);
    assert_eq!(foo.c, 2);

    assert!(foo.all(|x| x < 3));
    assert!(!foo.all(|x| x % 2 == 0));
}

#[test]
fn any() {
    let foo = Foo::from_fn(|_, i| i);
    assert_eq!(foo.a, 0);
    assert_eq!(foo.b, 1);
    assert_eq!(foo.c, 2);

    assert!(foo.any(|x| x < 3));
    assert!(foo.any(|x| x % 2 == 0));
}

#[test]
fn fold() {
    // Test case 1: Sum of numbers with initial value
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let sum = numbers.fold(0, |a, b| a + b);
    assert_eq!(sum, 9);

    // Test case 2: Multiplication of numbers with initial value
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let product = numbers.fold(1, |a, b| a * b);
    assert_eq!(product, 24);

    // Test case 3: Concatenate strings with initial value
    let words = Foo {
        a: "Hello",
        b: " World",
        c: "!",
    };
    let sentence = words.fold(String::new(), |mut acc, x| {
        acc.push_str(x);
        acc
    });
    assert_eq!(sentence, "Hello World!");
}

#[test]
fn for_each() {
    // Test case 1: Collect values into a vector using a side effect
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let mut result = Vec::new();
    numbers.for_each(|x| result.push(x));
    assert_eq!(result, vec![2, 3, 4]);

    // Test case 2: Calculate sum using side effects
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let mut sum = 0;
    numbers.for_each(|x| sum += x);
    assert_eq!(sum, 9);

    // Test case 3: Concatenate strings using a side effect
    let words = Foo {
        a: "Hello",
        b: " World",
        c: "!",
    };
    let mut sentence = String::new();
    words.for_each(|word| sentence.push_str(word));
    assert_eq!(sentence, "Hello World!");
}

#[test]
fn from_fn() {
    // Test cast 1: use field name
    let foo = Foo::from_fn(|name, _| name);
    assert_eq!(foo.a, "a");
    assert_eq!(foo.b, "b");
    assert_eq!(foo.c, "c");

    // Test cast 2: use field name with complex names
    let foo = ComplexNames::from_fn(|name, _| name);
    assert_eq!(foo.daipzjdpazjd_damkzjda, "daipzjdpazjd_damkzjda");
    assert_eq!(foo.zaidazpdjazp, "zaidazpdjazp");

    // Test cast 3: use field's index
    let foo = Foo::from_fn(|_, i| i);
    assert_eq!(foo.a, 0);
    assert_eq!(foo.b, 1);
    assert_eq!(foo.c, 2);
}

#[test]
fn map() {
    let numbers = Foo { a: 2, b: 3, c: 4 };

    let double = numbers.map(|i| i * 2);
    assert_eq!(double.a, 4);
    assert_eq!(double.b, 6);
    assert_eq!(double.c, 8);
}

#[test]
fn reduce() {
    // Test case 1: Sum of numbers
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let sum = numbers.reduce(|a, b| a + b);
    assert_eq!(sum, 9);

    // Test case 2: Multiplication of numbers
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let product = numbers.reduce(|a, b| a * b);
    assert_eq!(product, 24);
}

#[test]
pub fn unzip() {
    let values = Foo {
        a: ("Hello", 2),
        b: ("World", 3),
        c: ("!", 4),
    };

    let (words, numbers) = values.unzip();

    assert_eq!(
        words,
        Foo {
            a: "Hello",
            b: "World",
            c: "!"
        }
    );
    assert_eq!(numbers, Foo { a: 2, b: 3, c: 4 });
}

#[test]
pub fn zip() {
    let words = Foo {
        a: "Hello",
        b: "World",
        c: "!",
    };

    let numbers = Foo { a: 2, b: 3, c: 4 };

    let values = words.zip(numbers);

    assert_eq!(
        values,
        Foo {
            a: ("Hello", 2),
            b: ("World", 3),
            c: ("!", 4),
        }
    );
}
