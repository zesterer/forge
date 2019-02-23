use forge::Engine;

fn main() {
    let result = Engine::default()
        .execute(r#"

var c = 0;
while c < 10 {
    print "Iteration " + c + "!";
    if c % 2 == 0 {
        print "Even!";
    } else {
        print "Odd!";
    }
    c = c + 1;
}
var x = 0;
while true {
    print x;
    x = x + 1;
}

        "#)
        .map_err(|e| print!("{}", e));

    println!("Result = {:?}", result);
}

