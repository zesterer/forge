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
fn comments() {
    let mut engine = forge::Engine::default();

    assert!(engine.eval(r#"# A comment"#).unwrap() == Value::Null);
    assert!(engine.eval(r#"147.5 # Another # comment!"#).unwrap() == 147.5);
}

#[test]
fn equivalence() {
    let mut engine = forge::Engine::default();

    assert!(engine.eval(r#"12.34 == 12.34"#).unwrap() == true);
    assert!(engine.eval(r#""foo" == "foo""#).unwrap() == true);
    assert!(engine.eval(r#"true == true"#).unwrap() == true);

    assert!(engine.eval(r#"12.34 == null"#).unwrap() == false);
    assert!(engine.eval(r#""foo" == null"#).unwrap() == false);
    assert!(engine.eval(r#"true  == null"#).unwrap() == false);
}

#[test]
fn stringify() {
    let mut engine = forge::Engine::default();

    assert!(engine.eval(r#""12.34" == "" + 12.34"#).unwrap() == true);
    assert!(engine.eval(r#""true" == "" + true"#).unwrap() == true);
    assert!(engine.eval(r#""null" == "" + null"#).unwrap() == true);
}

#[test]
fn arithmetic() {
    let mut engine = forge::Engine::default();

    assert!(engine.eval(r#"-5"#).unwrap() == -5.0);

    assert!(engine.eval(r#"1 + 2"#).unwrap() == 3.0);
    assert!(engine.eval(r#"-5 + 2"#).unwrap() == -3.0);
    assert!(engine.eval(r#"-5 + -3"#).unwrap() == -8.0);

    assert!(engine.eval(r#"1 - 2"#).unwrap() == -1.0);
    assert!(engine.eval(r#"4 - 1"#).unwrap() == 3.0);

    assert!(engine.eval(r#"4 * 3"#).unwrap() == 12.0);
    assert!(engine.eval(r#"3 * -5"#).unwrap() == -15.0);
    assert!(engine.eval(r#"-7 * -2"#).unwrap() == 14.0);

    assert!(engine.eval(r#"7 / 7"#).unwrap() == 1.0);
    assert!(engine.eval(r#"1 / 7"#).unwrap() == 1.0 / 7.0);
    assert!(engine.eval(r#"7 / -1"#).unwrap() == -7.0);
    assert!(engine.eval(r#"1 / 2"#).unwrap() == 0.5);
}
