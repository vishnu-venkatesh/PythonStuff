use std::str::Chars;
use std::iter::Peekable;
use std::fmt::Debug;
//use std::fmt::Display;
use std::collections::HashMap;

/// This enum is for the tokens used by the calculator.
/// Right now it only handles integers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Token {
    Number {val: i64},
    Variable {handle: usize},
    Equals,
    Plus,
    UnaryMinus,
    Minus,
    Mult,
    Div,
    OpenParen,
    CloseParen,
    Dollar,
    EndOfInput,
    Invalid
}


impl Default for Token {
    fn default() -> Self {Token::Invalid}
}


impl Token {
    /// Returns the precedence of an operator token. For non-operator tokens, returns the
    /// value of 0. Higher precedence number -> higher precedence (needs to be done first)
    fn prec(&self) -> u32 {
        match *self {
            Token::CloseParen => 20,
            Token::Plus | Token::Minus => 100,
            Token::Mult | Token::Div => 1000,
            Token::UnaryMinus => 10000,
            Token::OpenParen => 20000,
            _ => 0,
        }
    }

    /// Returns the arity of an operator. Arity is the number of arguments that an operator
    /// needs to do its work.
    fn arity(&self) -> usize {
        match *self {
            Token::Plus | Token::Minus | Token::Mult | Token::Div => 2,
            Token::UnaryMinus => 1,
            _ => 0,
        }
    }
    
    /// Returns true if the token is an operator. Parentheses are also treated as operators.
    fn is_oper(&self) -> bool {
        match *self {
            Token::Number{val: _} | Token::Variable{handle: _}  | Token::EndOfInput | Token::Invalid => false,
            _ => true,
        }
    }

    /// Returns true if the token is a right associative operator. Right associativity means
    /// the operations are chained from right to left, instead of being grouped from left to right.
    fn is_right_assoc(&self) -> bool {
        match *self {
            Token::UnaryMinus => true,
            _ => false,
        }
    }
}

struct Tokenizer<'a> {

    chars: Peekable<Chars<'a>>,
    /// last_token is used to disambiguate the meaning of symbols like '-' (could be unary or
    /// binary.
    last_token: Token,    
    returned_first_tok: bool,
}

impl<'a> Tokenizer<'a> {
    fn new(in_str: &'a str) -> Self {
        Tokenizer {
            chars: in_str.chars().peekable(),
            last_token: Token::Invalid,
            returned_first_tok: false,
        }
    }

    fn next(&mut self, var_table: &mut HashMap<String, usize>, next_var_handle: &mut usize) -> Token {
        match self.last_token {
            Token::EndOfInput => { return Token::EndOfInput; }
            _ => {}
        }
        // We need this variable because we want to skip over unsupported chars 
        let mut tok_found; 
        loop {
            tok_found = true; // reset it at the beginning of the loop.
            if let Some(ch) = self.chars.peek() {
                match ch {
                    '0'..='9' => {
                        let mut val: i64 = 0;
                        while let Some(dig) = self.chars.peek() {
                            match dig {
                                '0'..='9' => {
                                    val *= 10;
                                    val += dig.to_digit(10).unwrap() as i64;
                                    self.chars.next();
                                },
                                _ => { break; }
                            }
                        }
                        // Note: Here we've already stopped at a character that
                        // cannot be part of a number. So we can return right away
                        // without calling self.chars.next()
                        self.last_token = Token::Number{val: val};
                        self.returned_first_tok = true;
                        return self.last_token;
                    },
                    'a'..='z' | 'A'..='Z' => {
                        let mut var_str = String::new();
                        while let Some(letter) = self.chars.peek() {
                            match letter {
                                'a'..='z' | 'A'..='Z' =>  {
                                    var_str.push(*letter);
                                    self.chars.next();
                                },
                                _ => {break;}
                            }
                        }

                        let var_handle = if let Some(old_var_handle) = var_table.get(&var_str) {
                            *old_var_handle
                        } else {
                            var_table.insert(var_str, *next_var_handle);
                            *next_var_handle += 1;
                            *next_var_handle-1
                        };
                        self.last_token = Token::Variable{handle: var_handle};
                        self.returned_first_tok = true;
                        return self.last_token;
                    }
                    '+' => {
                        self.last_token = Token::Plus;
                    },
                    '-' => {
                        if !self.returned_first_tok {
                            self.last_token = Token::UnaryMinus;
                        } else {
                            let minus = match self.last_token {
                                Token::Number{val: _} | Token::Variable{handle:_} | Token::CloseParen => Token::Minus,
                                _ => Token::UnaryMinus,
                            };
                            self.last_token = minus; 
                        }
                    },
                    '*' => {
                        self.last_token = Token::Mult;
                    },
                    '/' => {
                        self.last_token = Token::Div;
                    },
                    '(' => {
                        self.last_token = Token::OpenParen;
                    },
                    ')' => {
                        self.last_token = Token::CloseParen;
                    },
                    '=' => {
                        self.last_token = Token::Equals;
                    },
                    '$' => {
                        self.last_token = Token::Dollar;
                    },
                    _ => {tok_found = false;},
                }
                self.chars.next();
                if tok_found {
                    self.returned_first_tok = true;
                    return self.last_token;
                }
            } else {
                self.returned_first_tok = true;
                self.last_token = Token::EndOfInput;
                return Token::EndOfInput;
            }
        };
    }
}

struct Stack<T> {
    st: Vec<T>,
    ptr: usize, // points to the next available elem
}

#[derive(Debug, PartialEq, Eq)]
enum StackError {
    Overflow,
    Underflow,
    TopOfEmpty,
}

impl StackError {
    pub fn to_string(&self) -> String {
        match self {
            StackError::Overflow => "Stack overflow".to_string(),
            StackError::Underflow => "Stack underflow".to_string(),
            StackError::TopOfEmpty => "Empty stack has no top".to_string(),
        }
    }
}

impl From<StackError> for String {
    fn from (se: StackError) -> Self {
        se.to_string()
    }
}

impl<T: Copy + Default + Debug > Stack<T> {
    fn new(cap: usize) -> Self {
        let mut ret = Stack { 
            st: Vec::<T>::with_capacity(cap), 
            ptr: 0,
        };
        for _i in 0..cap {
            ret.st.push(T::default());
        }
        ret
    }

    fn push(&mut self, t: T) -> Result<(), StackError>{
        if self.ptr >= self.st.len() {
            return Err(StackError::Overflow); 
        }
        self.st[self.ptr] = t; 
        self.ptr += 1;
        Ok(())
    }

    fn pop(&mut self) -> Result<T, StackError> {
        if self.ptr == 0 {
            return Err(StackError::Underflow);
        }
        self.ptr -= 1;
        // using replace here so that when we pop an element
        // we don't leave stale values on the stack.
        Ok(std::mem::replace(&mut self.st[self.ptr], T::default()))
    }
    
    fn top(&self) -> Result<T, StackError> {
        if self.ptr == 0 {
            return Err(StackError::TopOfEmpty);
        }
        Ok(self.st[self.ptr-1])
    }

    fn empty(&self) -> bool {
        self.size() < 1
    }

    fn size(&self) -> usize {
        self.ptr
    }
    
    fn borrow_n(&self, n: usize) -> Option<&[T]> {
        if n > self.ptr {
            return None;
        }
        Some(&self.st[self.ptr-n..self.ptr])
    }

    fn to_string(&self) -> String {
        format!("{:?} ptr = {}", self.st, self.ptr).to_string()
    }
}

pub struct ExprEvaluator {
    op_stack: Stack<Token>,
    value_stack: Stack<i64>,
    var_map: HashMap<usize, i64>,
    var_table: HashMap<String, usize>,
    next_var_handle: usize,
    in_debug_mode: bool,

}

fn apply_op(op: Token, args: &[i64]) -> i64 {
        match op {
            Token::Plus => {args[0] + args[1]},
            Token::Minus => {args[0] - args[1]},
            Token::Mult => {args[0] * args[1]},
            Token::Div => {args[0] / args[1]},
            Token::UnaryMinus => -args[0],
            _ => panic!(format!("Unsupported operation: {:?}", op)),  
        }  
}

impl ExprEvaluator {
    pub fn new(op_stack_cap: usize, value_stack_cap: usize) -> Self {
        ExprEvaluator {
            op_stack: Stack::<Token>::new(op_stack_cap),
            value_stack: Stack::<i64>::new(value_stack_cap),
            var_map: HashMap::<usize, i64>::new(),
            var_table: HashMap::<String, usize>::new(),
            next_var_handle: 0,
            in_debug_mode: false,
        }
    }

    pub fn vars_and_values_as_string(&self) -> String {
        let mut ret = String::new();
        for (var_name, var_handle) in self.var_table.iter() {
            // Note: The tokenize inserts variables into the var table, regardless
            //       of whether or not they are being defined. So if the variable
            //       happens to be unbound, it will still be in the var table,
            //       but will not exist in the var_map with a value. So check for it.
            if let Some(value) = self.var_map.get(var_handle) {
                ret.push_str(&format!("{} = {}\n", &var_name, value));
            }
        }
        ret
    }

    fn pop_from_value_stack(&mut self, n: usize) -> bool {
        let mut i = 0;
        while let Ok(_) = self.value_stack.top() {
            if i == n {
                break;
            }
            self.value_stack.pop().expect("Did not check for empty before popping!");
            i += 1;
        }
        i == n
    }

    fn print_stacks(&self) {
        println!("op_stack: {}", self.op_stack.to_string());
        println!("value_stack: {}", self.value_stack.to_string());  
    }
    
    fn do_remaining_ops(&mut self) -> Result<(), String> {
        while let Ok(op) = self.op_stack.pop() {
            if self.in_debug_mode {
                println!("Applying op = {:?}", op);
                self.print_stacks();
            }
            if op == Token::OpenParen {
                return Err("Mismatched parentheses!".to_string());
            }
            let temp_res = 
                if let Some(args) = self.value_stack.borrow_n(op.arity()) {
                    apply_op(op, args)
                } else {
                    if self.in_debug_mode {
                        println!("op_stack: {}", self.op_stack.to_string());
                        println!("value_stack: {}", self.value_stack.to_string());
                    }
                    return Err("Syntax error".to_string()); 
                }; 
            self.pop_from_value_stack(op.arity());
            self.value_stack.push(temp_res)?; 
            if self.in_debug_mode {
                println!("After applying op = {:?}", op);
                self.print_stacks();
            }
        }
        Ok(())
    }

    fn handle_operator(&mut self, tok: Token) -> Result<(), String> {
        if self.in_debug_mode {
            println!("Encountered tok = {:?}", tok);
        }
        if let Ok(mut top_op) = self.op_stack.top() {
            while top_op != Token::OpenParen && (tok.prec() <= top_op.prec() || (tok.is_right_assoc() && tok.prec() < top_op.prec())) {
                if self.in_debug_mode {
                    println!("Applying top_op = {:?} arity = {}", top_op, top_op.arity());
                    self.print_stacks();
                }
                let temp_res = 
                    if let Some(args) = self.value_stack.borrow_n(top_op.arity()) {
                        apply_op(top_op, args)
                    } else {
                        return Err("Syntax error: insufficient operands".to_string()); 
                    };
                self.pop_from_value_stack(top_op.arity());
                self.value_stack.push(temp_res).expect("Failed to push value onto value stack"); 
                self.op_stack.pop().expect("Op stack is empty!");
                if self.in_debug_mode {
                    println!("After Applying top_op = {:?}", top_op);
                    self.print_stacks();
                }
                top_op = match self.op_stack.top() {
                    Ok(t) => t,
                    Err(StackError::TopOfEmpty) => {break;},
                    _ => {panic!("Received unexpected stack error!");}
                };
            }
        }
        if tok == Token::CloseParen {
            match self.op_stack.pop() {
                Ok(Token::OpenParen) => {},
                _ => {return Err("Syntax error: Mismatched Parens".to_string());},
            };
        } else {
            match self.op_stack.push(tok) {
                Err(stack_err) => { return Err(stack_err.to_string()); },
                _ => {},
            };
        }
        Ok(())
    }

    fn clear_stacks(&mut self) {
        // Reset the evaluator
        while !self.op_stack.empty() {
            self.op_stack.pop().expect("Popping empty op stack!");
        }
        while !self.value_stack.empty() {
            self.value_stack.pop().expect("Popping empty value stack");
        } 
    }
    pub fn toggle_debug(&mut self) -> bool {
        self.in_debug_mode = !self.in_debug_mode;
        self.in_debug_mode
    }

    pub fn eval_statement(&mut self, expr_str: &str) -> Result<i64, String> {
        
        self.clear_stacks();

        let mut tokenizer = Tokenizer::new(expr_str);  
        
        let tok = tokenizer.next(&mut self.var_table, &mut self.next_var_handle);
        match tok {
            Token::Variable{handle} => {
                let eq_tok = tokenizer.next(&mut self.var_table, &mut self.next_var_handle);
                match eq_tok {
                    Token::Equals => {
                        let res = self.eval(&mut tokenizer)?;
                        self.var_map.insert(handle, res);
                        return Ok(res);
                    },
                    _ => {
                        if let Some(val) = self.var_map.get(&handle) {
                            if self.in_debug_mode {
                                println!("getting var {:?} with value of {:?}", handle, val);
                            }
                            match self.value_stack.push(*val) {
                                Err(stack_err) => { return Err(stack_err.to_string()); }, 
                                _ => {},
                            }  
                        } else {
                            return Err(format!("Unbound variable handle encountered: {:?}", handle));
                        }

                        if eq_tok.is_oper() {
                            if self.in_debug_mode {
                                println!("eq_tok = {:?}", eq_tok);
                            }
                            self.op_stack.push(eq_tok).expect(&format!("Unable to push tok = {:?}", eq_tok));
                        } else if eq_tok != Token::EndOfInput {
                            return Err(format!("Syntax error. token = {:?}", eq_tok));
                        }
                    }
                }
                if self.in_debug_mode {
                    println!("stacks for var expr");
                    self.print_stacks();
                    println!("end stacks for var expr");
                }
                let ret = self.eval(&mut tokenizer);
                
                return ret;
            },
            
            Token::Number{val} => {
                self.value_stack.push(val).expect("Unable to push number onto value stack");
            },
            
            Token::UnaryMinus => {
                self.op_stack.push(Token::UnaryMinus).expect("Unable to push unary minus onto op stack");
            },
            Token::EndOfInput => {},
            Token::OpenParen => {
                self.op_stack.push(Token::OpenParen).expect("Unable to push open paren onto op stack");
            },
             
            _ => { return Err(format!("Unexpected token {:?}", tok)); }
        }
        return self.eval(&mut tokenizer);
    }

    fn eval(&mut self, tokenizer: &mut Tokenizer) -> Result<i64, String> {
        

        let mut tok = tokenizer.next(&mut self.var_table, &mut self.next_var_handle);
        while tok != Token::Invalid && tok != Token::EndOfInput {
            if tok.is_oper() {
                match self.handle_operator(tok) {
                    Err(msg) => {return Err(msg);},
                    _ => {}
                }
            } else if let Token::Number{val} = tok {
                match self.value_stack.push(val) {
                    Err(stack_err) => { return Err(stack_err.to_string()); }, 
                    _ => {},
                }
            } else if let Token::Variable{handle} = tok {
                if let Some(val) = self.var_map.get(&handle) {
                    match self.value_stack.push(*val) {
                        Err(stack_err) => { return Err(stack_err.to_string()); }, 
                        _ => {},
                    }  
                } else {
                    return Err(format!("Unbound variable handle encountered: {:?}", handle));
                }
            }
            else {
                panic!(format!("Unsupported token {:?}", tok));
            }
            tok = tokenizer.next(&mut self.var_table, &mut self.next_var_handle);
        }
        if tok == Token::Invalid {
            panic!("Encountered invalid token!");
        }
        match self.do_remaining_ops() {
            Err(e) => {return Err(e.to_string()); },
            _ => {},
        }; 
        
        if let Ok(ret) = self.value_stack.pop() {
            if self.value_stack.empty() {
                Ok(ret)
            } else {
                Err("Syntax error: Operands remaining on value stack".to_string())
            }
        } else {
            Ok(0)
        }
    }
}

#[cfg(test)]
mod test {
    use super::Stack;
    use super::Token;
    use super::StackError;
    use super::Tokenizer;
    use super::ExprEvaluator; 
    use super::HashMap;

    #[test]
    fn basic_tokenizer_test() {
        let mut var_table = HashMap::<String, usize>::new();
        let mut next_var_handle: usize = 0; 

        let mut tokenizer = Tokenizer::new("-45+3*(3+2)abbc/-8"); 
        let expected_toks = vec![Token::UnaryMinus, 
                                 Token::Number{val:45}, 
                                 Token::Plus, 
                                 Token::Number{val:3},
                                 Token::Mult,
                                 Token::OpenParen,
                                 Token::Number{val:3},
                                 Token::Plus,
                                 Token::Number{val:2},
                                 Token::CloseParen,
                                 Token::Variable{handle: 0},
                                 Token::Div,
                                 Token::UnaryMinus,
                                 Token::Number{val:8},];
        let mut i = 0;
        let mut tok = tokenizer.next(&mut var_table, &mut next_var_handle);
        while tok != Token::EndOfInput {
            assert_eq!(tok, expected_toks[i]);
            i += 1;
            tok = tokenizer.next(&mut var_table, &mut next_var_handle);
        }
    }

    #[test]
    fn basic_stack_test() {
        let mut stack = Stack::<Token>::new(4);
      
        assert!(match stack.top() {
            Err(StackError::TopOfEmpty) => true,
            _ => false,
        });
        assert_eq!(stack.push(Token::Number{val: 3}), Ok(()));
        
        assert_eq!(stack.push(Token::Number{val: 6}), Ok(()));
        assert_eq!(stack.push(Token::OpenParen), Ok(()));
        assert_eq!(stack.push(Token::Plus), Ok(()));
        assert!(match stack.push(Token::CloseParen) {
            Err(StackError::Overflow) => true,
            _ => false,
        });

        assert!(match stack.top() {
            Ok(Token::Plus) => true,
            _ => false,
        });

        assert!(match stack.pop() {
            Ok(Token::Plus) => true,
            _ => false,
        }); 

        assert!(match stack.pop() {
            Ok(Token::OpenParen) => true,
            _ => false,
        });

        assert!(match stack.pop() {
            Ok(Token::Number{val: 6}) => true,
            _ => false,
        });

        assert!(match stack.pop() {
            Ok(Token::Number{val: 3}) => true,
            _ => false,
        });          

        assert!(stack.empty());
        
    }

    #[test]
    fn test_evaluator() {
        let mut evaluator = ExprEvaluator::new(8, 8);

        let mut ans = evaluator.eval(&mut Tokenizer::new("2+3"));
        println!("ans = {:?}", ans);
        assert!(match ans {
            Ok(5) => true,
            _ => false,
        });

        evaluator.clear_stacks();

        ans = evaluator.eval(&mut Tokenizer::new("2*3+-4*5+8/(3+1)"));
        println!("ans = {:?}", ans); 
        assert!(match ans {
            Ok(-12) => true,
            _ => false,
        });

        evaluator.clear_stacks(); 

        ans = evaluator.eval(&mut Tokenizer::new("2*3----4*5-8/(3+1)"));
        println!("ans = {:?}", ans); 
        assert!(match ans {
            Ok(24) => true,
            _ => false,
        }); 

        evaluator.clear_stacks(); 

        ans = evaluator.eval(&mut Tokenizer::new("((()()))")); 
        println!("ans = {:?}", ans);
        assert!(match ans {
            Ok(0) => true,
            _ => false,
        });
    }

    #[test]
    fn test_eval_statements() {
        let mut evaluator = ExprEvaluator::new(8, 8);

        let mut ans = evaluator.eval_statement("x=3");
        println!("ans = {:?}", ans);
        assert!(match ans {
            Ok(3) => true,
            Err(msg) => {
                println!("{}", &msg);
                false
            },
            Ok(x) => {
                println!("Expected 3, got {}", x);
                false 
            },
        });

        ans = evaluator.eval_statement("y=2");  
        println!("ans = {:?}", ans);
        assert!(match ans {
            Ok(2) => true,
            Err(msg) => {
                println!("{}", &msg);
                false
            },
            Ok(x) => {
                println!("Expected 2, got {}", x);
                false 
            }, 
        });

        ans = evaluator.eval_statement("z=x+y");
        println!("ans = {:?}", ans);
        assert!(match ans {
            Ok(5) => true,
            Err(msg) => {
                println!("{}", &msg);
                false
            },
            Ok(x) => {
                println!("Expected 5, got {}", x);
                false 
            }, 
        });

        ans = evaluator.eval_statement("x+y");
        println!("ans = {:?}", ans);
        assert!(match ans {
            Ok(5) => true,
            Err(msg) => {
                println!("{}", &msg);
                false
            },
            Ok(x) => {
                println!("Expected 5, got {}", x);
                false 
            }, 
        }); 
    } 
}

