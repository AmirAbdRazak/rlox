Lox Interpreter in rust, following Crafting Interpreters.

Has the basic stuff you would find in lox.

Invoking the lambda expression is a bit different.
```
fn test_print(print_fn) {
    print_fn("testing the print...");
}

test_print(|x| {
    print x;
})
```

Otherwise its same old same old.
