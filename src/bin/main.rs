use forge::Engine;

fn main() {
    Engine::default()
        .execute(r#"
print "Hello, world!";
trait
let x = 5;
print "Testing!";
@
class TestClass
"
print 47 + 8;
        "#)
        .unwrap_or_else(|e| print!("{}", e));
}

