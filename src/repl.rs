use std::io;
use std::io::Write;
use super::calc;

/// Core structure for the REPL
pub struct Repl {
    eval: calc::ExprEvaluator,    
}

impl Repl {

    /// Creates and returns a new Repl object
    pub fn new() -> Self {
        Repl { eval: calc::ExprEvaluator::new(32, 64), }
    }
    
    fn print_help() {
        println!("{}{}{}{}{}{}{}",
            "Here are the things you can do in BoogieCalc:\n",
            "    .help - Print's this help message\n",
            "    .quit - Exits the calculator\n",
            "    .show_vars - Shows variables in the current session\n",
            "    .toggle_debug - Toggles verbose debug logging\n",
            "    Evaluate an expression: 2*3+a\n",
            "    Assign i.e. z=3+2 - Makes a new variable z and gives it the value of rhs\n");
    }

    pub fn run(&mut self) { 
        println!("Welcome to BoogieCalc!");
        println!("Enter a statement or expression, or .quit to quit .help for help");
        let mut input = String::new();
        loop {
            print!("> ");
            io::stdout().flush().expect("Unable to flush stdout!");
            if let Ok(_n) = io::stdin().read_line(&mut input) {
                match input.trim() {
                    ".quit" => {break;},
                    ".show_vars" => {
                        let vars = self.eval.vars_and_values_as_string();
                        println!("============ Variables ============");
                        println!("{}", &vars);
                        println!("");
                    },
                    ".toggle_debug" => {
                        if self.eval.toggle_debug() {
                            println!("DEBUG ON");
                        } else {
                            println!("DEBUG OFF");
                        }
                    },
                    ".help" => {
                        Repl::print_help();
                    },
                    _ => {
                        let res = self.eval.eval_statement(input.as_ref());
                        match res {
                            Ok(val) => {println!("{}", val);}
                            Err(msg) => {println!("Error: {}", &msg);}
                        }
                    },
                }
                input.clear();
            }
        }
    }
}


