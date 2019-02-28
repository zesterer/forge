use std::{
    env,
    fs,
    io::prelude::*,
};
use forge::Engine;
use rustyline::Editor;

fn prompt() {
    println!("Welcome to the Forge prompt.");

    let mut engine = Engine::default();

    let mut rl = Editor::<()>::new();
    while let Ok(line) = rl.readline(">> ") {
        rl.add_history_entry(line.as_ref());

        let _ = engine.prompt(&line)
            .map(|val| val.map(|val| {
                println!("{}", val.get_display_text().unwrap_or("<value cannot be displayed>".to_string()))
            }))
            .map_err(|err| print!("{}", err));
    }
}

fn exec(fname: &str) {
    let mut code = String::new();
    match fs::File::open(fname) {
        Ok(mut file) => { file.read_to_string(&mut code).unwrap(); },
        Err(_) => println!("Could not open file '{}'", fname),
    }

    let mut engine = Engine::default();

    let _ = engine.exec(&code)
        .map_err(|err| print!("{}", err));
}

fn usage() {
    println!("Usage: forge [file]");
}

fn main() {
    match &env::args().nth(1) {
        None => prompt(),
        Some(arg) if env::args().count() == 2 => exec(arg),
        Some(_) => usage(),
    }
}
