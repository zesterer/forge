use forge;

#[test]
fn basic() {
    let mut engine = forge::Engine::default();

    panic!("Result: {:?}", engine.eval(r#"
        "world" == "world"
    "#));
}
