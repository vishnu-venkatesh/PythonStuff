mod calc;
mod repl;
fn main() {
    let mut my_repl = repl::Repl::new();
    my_repl.run();

}
