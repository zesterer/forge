use forge::Engine;

fn main() {
    let mut engine = Engine::build()
        .with_global("name", "Alex")
        .with_global("bag_weight", 46)
        .with_global("groceries", vec!["An apple", "A pear", "A carton of milk", "A box of eggs"])
        .with_global("finished", false)
        .finish();

    engine.exec(r#"
        print "My name is " + name;
        print "My bag weighs " + bag_weight + " Kg";

        print "In my bag I have:";
        for item in groceries {
            print item;
        }

        if finished {
            print "I've just finished shopping";
        } else {
            print "I've not finished shopping yet";
        }
    "#).unwrap();
}
