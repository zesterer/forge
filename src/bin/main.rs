use forge::{
    Engine,
    Obj,
};
use rustyline::Editor;

fn main() {
    let mut engine = Engine::default();

    println!("Welcome to the Forge prompt.");

    let mut rl = Editor::<()>::new();
    while let Ok(line) = rl.readline(">> ") {
        rl.add_history_entry(line.as_ref());
        let _ = engine.prompt(&line)
            .map(|val| val.map(|val| println!("{}", val.get_display_text())))
            .map_err(|err| print!("{}", err));
    }
}

