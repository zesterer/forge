use forge::{Engine, Obj};

#[derive(Debug)]
struct ShoppingBasket(Vec<&'static str>);

impl Obj for ShoppingBasket {}

fn main() {
    // Create an engine. Give it a custom value in the global scope
    let mut engine = Engine::build()
        .with_global("my_basket", ShoppingBasket(vec!["apples", "oranges", "pears"]))
        .finish();

    // Execute some code to realias the value
    // Note that this is just to demonstrate that the object can be interacted with
    engine
        .exec("var my_basket_ref = my_basket;")
        .unwrap();

    // Display the new alias we created previously
    println!(
        "my_basket_ref = {}",
        engine
            .take("my_basket_ref")
            .unwrap(),
    );
}
