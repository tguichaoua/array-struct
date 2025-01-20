use array_struct::array_struct;

array_struct! {
    struct Foo {
        a, b, c
    }
}

#[test]
fn try_fold() {
    // Test case 1: Successful accumulation
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let result: Result<i32, &str> = numbers.try_fold(0, |acc, x| Ok(acc + x));
    assert_eq!(result, Ok(9));

    // Test case 2: Early termination on an error
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let result: Result<i32, &str> = numbers.try_fold(0, |acc, x| {
        if x == 3 {
            Err("Error at 3")
        } else {
            Ok(acc + x)
        }
    });
    assert_eq!(result, Err("Error at 3"));
}

#[test]
fn try_for_each() {
    // Test case 1: Successful iteration
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let mut sum = 0;
    let result: Result<(), &str> = numbers.try_for_each(|x| {
        sum += x;
        Ok(())
    });
    assert_eq!(result, Ok(()));
    assert_eq!(sum, 9);

    // Test case 2: Early termination on an error
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let mut sum = 0;
    let result: Result<(), &str> = numbers.try_for_each(|x| {
        if x == 4 {
            Err("Error at 4")
        } else {
            sum += x;
            Ok(())
        }
    });
    assert_eq!(result, Err("Error at 4")); // Expected early termination with error
    assert_eq!(sum, 5); // Sum should only include 2 + 3
}

#[test]
fn try_from_fn() {
    // Test case 1: Successful iteration
    let result: Result<Foo<_>, &str> = Foo::try_from_fn(|name, _| Ok(name));
    assert_eq!(
        result,
        Ok(Foo {
            a: "a",
            b: "b",
            c: "c"
        })
    );

    // Test case 2: Early termination on an error
    let result: Result<Foo<_>, &str> = Foo::try_from_fn(|name, i| {
        if i == 2 {
            Err("unexpected index 2")
        } else {
            Ok(name)
        }
    });
    assert_eq!(result, Err("unexpected index 2"));
}

#[test]
fn try_map() {
    // Test case 1: Successful iteration
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let double: Result<Foo<i32>, &str> = numbers.try_map(|i| Ok(i * 2));
    assert_eq!(double, Ok(Foo { a: 4, b: 6, c: 8 }));

    // Test case 2: Early termination on an error
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let double: Result<Foo<i32>, &str> = numbers.try_map(|i| {
        if i < 4 {
            Ok(i * 2)
        } else {
            Err("value too big")
        }
    });
    assert_eq!(double, Err("value too big"));
}

#[test]
fn try_reduce() {
    // Test case 1: Successful reduction
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let result: Result<i32, &str> = numbers.try_reduce(|acc, x| Ok(acc + x));
    assert_eq!(result, Ok(9));

    // Test case 2: Early termination on an error
    let numbers = Foo { a: 2, b: 3, c: 4 };
    let result: Result<i32, &str> = numbers.try_reduce(|acc, x| {
        if x == 3 {
            Err("Error at 3")
        } else {
            Ok(acc + x)
        }
    });
    assert_eq!(result, Err("Error at 3"));
}
