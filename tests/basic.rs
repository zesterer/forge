use forge;

#[test]
fn basic() {
    let mut engine = forge::Engine::default();

    println!("Result: {:?}", engine.eval(r#"
        "world" == "world"
    "#));
}
