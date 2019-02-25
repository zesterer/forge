use forge::{Engine, Obj};

#[derive(Debug)]
struct ShoppingBasket(Vec<&'static str>);

impl Obj for ShoppingBasket {
    fn get_type_name(&self) -> String { "ShoppingBasket".to_string() }
}

fn main() {
    let mut engine = Engine::build()
        .with_global_custom("my_basket", ShoppingBasket(vec!["apples", "oranges", "pears"]))
        .finish();

    println!(
        "my_basket = {:?}",
        engine
            .take("my_basket")
            .unwrap()
            .as_custom()
            .unwrap(),
    );
}
