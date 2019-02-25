use forge::Engine;

fn main() {
    let mut engine = Engine::build()
        .with_global("say_hello", || println!("Hello, world!"))
        .with_global("get_meaning_of_life", || 42)
        .finish();

    engine
        .exec(r#"
        say_hello();
        print "The meaning of life is " + get_meaning_of_life();
        "#)
        .unwrap();
}
