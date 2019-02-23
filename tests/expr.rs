use forge::{Engine, Value};

#[test]
fn literals() {
    let mut engine = forge::Engine::default();

    assert!(engine.eval(r#"12.34"#).unwrap() == 12.34);
    assert!(engine.eval(r#""foo""#).unwrap() == "foo");
    assert!(engine.eval(r#"true"#).unwrap() == true);

    assert!(engine.eval(r#"12.34"#).unwrap() != 43.21);
    assert!(engine.eval(r#""foo""#).unwrap() != "bar");
    assert!(engine.eval(r#"true"#).unwrap() != false);
}

#[test]
fn equivalence() {
    let mut engine = forge::Engine::default();

    assert!(engine.eval(r#"12.34 == null"#).unwrap() == false);
    assert!(engine.eval(r#""foo" == null"#).unwrap() == false);
    assert!(engine.eval(r#"true  == null"#).unwrap() == false);
}
