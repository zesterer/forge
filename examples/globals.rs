use forge::Engine;

fn main() {
    let mut engine = Engine::build()
        .with_global("x", 1337)
        .with_global("y", 2.5)
        .with_global("fruit", "orange")
        .with_global("apocalypse_occured_yet", false)
        .finish();

    println!("x = {}", engine.eval("x").unwrap());
    println!("y = {}", engine.eval("y").unwrap());
    println!("fruit = {}", engine.eval("fruit").unwrap());
    println!("apocalypse_occured_yet = {}", engine.eval("apocalypse_occured_yet").unwrap());
}
