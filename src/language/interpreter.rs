use super::*;
use super::InterpreterError as Error;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

macro_rules! add_variable_operator {
    ($op_name:ident, $op:tt, $op_str:expr) => {
        impl Variable {
            pub fn $op_name(self, rhs: Variable) -> Result<Variable, Error> {
                match self {
                    Variable::Int(lhs) => {
                        match rhs {
                            Variable::Int(rhs) => {
                                Ok(Variable::Int(lhs $op rhs))
                            },
                            _ => {
                                Err(Error::InvalidOperator {
                                    operator: $op_str,
                                    lhs: "Int",
                                    rhs: "Non-Int",
                                })
                            }
                        }
                    },
                    Variable::Float(lhs) => {
                        match rhs {
                            Variable::Float(rhs) => {
                                Ok(Variable::Float(lhs $op rhs))
                            },
                            _ => {
                                Err(Error::InvalidOperator {
                                    operator: $op_str,
                                    lhs: "Float",
                                    rhs: "Non-Float",
                                })
                            }
                        }
                    },
                    _ => {
                        Err(Error::InvalidOperator {
                            operator: $op_str,
                            lhs: "Non-Int/Float",
                            rhs: "Irrelevant",
                        })
                    }
                }
            }
        }
    };
}

#[macro_export]
macro_rules! function {
    ($f:ident{$($arg:ident),*}) => {
        ($f, 
            language::parser::Function { 
                parameters: vec![$(Var { var_type: language::VarType::$arg, len: 1 }),*], 
                return_value: language::Var { 
                    var_type: language::VarType::Null, 
                    len: 1 
                } 
            }
        )
    };
}

type Locals = std::collections::HashMap<String, usize>;
type Variables = Vec<Variable>;
type Functions = std::collections::HashMap<String, Function>;

#[derive(Clone, Debug)]
pub enum Function {
    Rust(fn(Vec<Variable>) -> Variable, Vec<Var>),
    Native(Vec<Instruction>, Vec<(String, Var)>),
}

impl Function {
    pub fn run<'s>(
        &'s self, 
        mut scope: Scope<'s>, 
        stack: Vec<Token> 
    ) -> Result<(Variable, usize), Error> {
        match self {
            Function::Rust(function, parameters) => {
                if stack.len() > parameters.len() {
                    return Err(Error::InvalidFunctionCall("Too many parameters"));
                } else if stack.len() < parameters.len() {
                    return Err(Error::InvalidFunctionCall("Too few parameters"));
                } 

                let mut params = Vec::new();

                for s in stack {
                    params.push(s.resolve(&mut scope)?);
                }

                Ok((function(params), 0))
            },
            Function::Native(instructions, parameters) => {
                if stack.len() > parameters.len() {
                    return Err(Error::InvalidFunctionCall("Too many parameters"));
                } else if stack.len() < parameters.len() {
                    return Err(Error::InvalidFunctionCall("Too few parameters"));
                } 

                scope.instructions = instructions;
                let mut locals = HashMap::new();

                for (v, p) in stack.iter().zip(parameters.iter()) {
                    let index = scope.stack.borrow().len();
                    scope.stack.borrow_mut().push(Variable::Null);
                    
                    let mut value = v.resolve(&mut scope)?;
                    
                    if let Variable::Ref(Ref::Local(reference)) = value {
                        value = Variable::Ref(Ref::Global(*scope.locals.get(&reference).unwrap()));
                    }

                    scope.stack.borrow_mut()[index] = value;
                    locals.insert(p.0.clone(), index);
                    scope.stack_len += 1;
                }

                scope.locals = locals;

                let var = scope.run()?;

                Ok(var)
            }
        }
    } 
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ref {
    Global(usize),
    Local(String),
    Heap(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Variable {
    Any,
    Ref(Ref),
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Null,
    FunctionRef(String),
    Slice(usize, usize, usize),
    Array(usize),
    Struct(HashMap<String, usize>),
}

impl Variable {
    pub fn parse(mut string: String, vars: &HashMap<String, Var>) -> Result<(Self, Var), super::ParseError> {
        let mut lexer = super::Lexer::new(&mut string);

        if lexer.find("\"") {
            let string = lexer.get("\"");

            match string {
                Some(string) => return Ok((Variable::String(string), Var { var_type: VarType::String, len: 1 })),
                None => return Err(super::ParseError::StringNotEnded),
            }
        }

        if let Ok(int) = string.parse::<i32>() {
            return Ok((Variable::Int(int), Var { var_type: VarType::Int, len: 1 }));
        }

        if let Ok(float) = string.parse::<f32>() {
            return Ok((Variable::Float(float), Var { var_type: VarType::Float, len: 1 }));
        }

        if string.as_str() == "true" {
            return Ok((Variable::Bool(true), Var { var_type: VarType::Bool, len: 1 }));
        }

        if string.as_str() == "false" {
            return Ok((Variable::Bool(false), Var { var_type: VarType::Bool, len: 1 }));
        }

        if super::parse_reference(&string) {
            if let Some(var) = vars.get(&string) { 
                return Ok((
                    Variable::Ref(
                        Ref::Local(string),
                    ),
                    Var { var_type: VarType::Ref(Box::new(var.clone())), len: 1 }
                ));
            } else {
                return Err(super::ParseError::UnsetVariable(string));
            }
        }

        Err(super::ParseError::VariableParseFailiure(string))
    }

    pub fn free(&self, scope: &mut Scope) {
        match self {
            Variable::Slice(location, elems, elem_len) => {
                for elem in 0..*elems * elem_len {
                    scope.heap.borrow_mut().remove(&(location + elem));
                }
            },
            _ => ()
        }
    }
}
add_variable_operator!(add, +, "Add");
add_variable_operator!(sub, -, "Sub");
add_variable_operator!(mul, *, "Mul");
add_variable_operator!(div, /, "Div");



#[derive(Clone, Debug)]
pub enum Instruction {
    InitVariable(String, Token),
    SetVariable(Token, Token),
    FunctionCall(Token),
    Return(Token, usize),
    If(Token, Vec<Instruction>),
    AddFunction(String, Function),
}

#[derive(Clone, Debug)]
pub enum Token {
    Variable(Variable),
    Deref(Box<Token>, usize),
    FunctionCall(Box<Token>, Vec<Token>),
    AccessField(Box<Token>, String),
    Index(Box<Token>, Box<Token>, usize),
    InitStruct(Vec<(String, Token)>),
    InitArray(Vec<Token>),
    Add(Box<Token>, Box<Token>),
    Sub(Box<Token>, Box<Token>),
    Mul(Box<Token>, Box<Token>),
    Div(Box<Token>, Box<Token>),
    Eq(Box<Token>, Box<Token>),
    Neq(Box<Token>, Box<Token>),
    Not(Box<Token>),
}

impl Token {
    pub fn resolve(&self, scope: &mut Scope) -> Result<Variable, Error> {
        match self {
            Token::Variable(variable) => {
                Ok(variable.clone())
            },
            Token::Deref(token, len) => {
                if let Variable::Ref(reference) = token.resolve(scope)? {
                    let location = match reference {
                        Ref::Local(reference) => *scope.locals.get(&reference).unwrap(),
                        Ref::Global(reference) => reference,
                        Ref::Heap(location) => {

                            let mut new_location = 0;

                            let heap = scope.heap.borrow();
                            let mut peekable = heap.iter().peekable();

                            while let Some((location, _)) = peekable.next() {
                                if let Some((next_location, _)) = peekable.peek() {
                                    if **next_location - location >= *len {
                                        new_location = **next_location - location;
                                        break;
                                    }
                                } else {
                                    new_location = *location;
                                    break;
                                }
                            }

                            for i in 1..*len { 
                                let variable = scope.heap.borrow().get(&(location + i)).unwrap().clone();
                                scope.heap.borrow_mut().insert(new_location + i, variable);
                            }
     
                            return Ok(scope.heap.borrow().get(&location).unwrap().clone());
                        }
                    };

                    for i in 1..*len {
                        scope.stack_len += 1;

                        let variable = scope.stack.borrow()[location + i].clone();
                        scope.stack.borrow_mut().push(variable);
                    }

                    return Ok(scope.stack.borrow()[location].clone());
                } else {
                    return Err(Error::DereferenceingNonReference);
                }
            }
            Token::FunctionCall(function, parameters) => {
                if let Variable::FunctionRef(function_ref) = function.resolve(scope)? {
                    if let Some(function) = scope.functions.borrow().get(&function_ref) {
                        let (return_value, len) = 
                            function.clone().run(scope.child(&Vec::new()), parameters.clone())?;

                        scope.stack_len += len;

                        return Ok(return_value);
                    } else {
                        return Err(Error::InvalidFunctionCall("Function not declared"));
                    }
                } else {
                    return Err(Error::InvalidFunctionCall("Trying to call non-function"));
                }
            },
            Token::AccessField(object, field) => {
                if let Token::Deref(reference, _) = &**object {
                    if let Variable::Ref(reference) = reference.resolve(scope)? {
                        let mut heap = scope.heap.borrow_mut();
                        let mut stack = scope.stack.borrow_mut();

                        let (object, location) = match reference {
                            Ref::Local(reference) => {
                                (&mut stack[*scope.locals.get(&reference).unwrap()], 
                                 *scope.locals.get(&reference).unwrap())
                            },
                            Ref::Global(reference) => {
                                (&mut stack[reference], reference)
                            },
                            Ref::Heap(reference) => {
                                (heap.get_mut(&reference).unwrap(), reference)
                            },
                        };

                        if let Variable::Struct(object) = object {
                            if let Some(field) = object.get(field) {
                                let field = field.clone();

                                return Ok(Variable::Ref(Ref::Global(field + location)));
                            } else {
                                return Err(Error::IndexError("Trying to access non-field"));
                            }
                        } else {
                            return Err(Error::IndexError("Trying to access field on non-struct"));
                        }
                    } else {
                        return Err(Error::DereferenceingNonReference);
                    }
                } else {
                    return Err(Error::IndexError("Cannot access field on non-references"));
                }
            },
            Token::Index(array, index, element_len) => {
                let array = if let Token::Deref(reference, _) = &**array {
                    reference.resolve(scope)?
                } else {
                    array.resolve(scope)?
                };


                if let Variable::Ref(reference) = array {
                    let index = index.resolve(scope)?;

                    let mut heap = scope.heap.borrow_mut();
                    let mut stack = scope.stack.borrow_mut();

                    let (array, location) = match reference {
                        Ref::Local(reference) => {
                            (&mut stack[*scope.locals.get(&reference).unwrap()], 
                             *scope.locals.get(&reference).unwrap())
                        },
                        Ref::Global(reference) => {
                            (&mut stack[reference], reference)
                        },
                        Ref::Heap(reference) => {
                            (heap.get_mut(&reference).unwrap(), reference)
                        },
                    };

                    if let Variable::Array(_) = array {
                        if let Variable::Int(index) = index {
                            return Ok(Variable::Ref(Ref::Global(location + 1 + index as usize * *element_len)));
                        } else {
                            return Err(Error::IndexError("Arrays are only indexed by integers"));
                        }
                    } else {
                        return Err(Error::IndexError("Tried to index into a non-array"));
                    }
                } else { 
                    return Err(Error::IndexError("Tried to index into a non-array"));
                }
            },
            Token::InitStruct(fields) => {
                let mut map = HashMap::new();

                let struct_index = scope.stack.borrow().len();

                for (field, token_tree) in fields {
                    scope.stack_len += 1;

                    let internal_index = scope.stack.borrow().len() - struct_index + 1;

                    let index = scope.stack.borrow().len();
                    scope.stack.borrow_mut().push(Variable::Null);
                    scope.stack.borrow_mut()[index] = token_tree.resolve(scope)?;

                    map.insert(field.clone(), internal_index);
                }

                return Ok(Variable::Struct(map));
            },
            Token::InitArray(elements) => {
                for i in elements {
                    scope.stack_len += 1;

                    let index = scope.stack.borrow().len();
                    scope.stack.borrow_mut().push(Variable::Null);
                    scope.stack.borrow_mut()[index] = i.resolve(scope)?;
                }

                return Ok(Variable::Array(elements.len()));
            },
            Token::Add(lhs, rhs) => {
                return Ok(lhs.resolve(scope)?.add(rhs.resolve(scope)?)?);
            },
            Token::Sub(lhs, rhs) => {
                return Ok(lhs.resolve(scope)?.sub(rhs.resolve(scope)?)?);
            }
            Token::Mul(lhs, rhs) => {
                return Ok(lhs.resolve(scope)?.mul(rhs.resolve(scope)?)?);
            },
            Token::Div(lhs, rhs) => {
                return Ok(lhs.resolve(scope)?.div(rhs.resolve(scope)?)?);
            },
            Token::Eq(lhs, rhs) => {
                return Ok(Variable::Bool(lhs.resolve(scope)? == rhs.resolve(scope)?));
            },
            Token::Neq(lhs, rhs) => {
                return Ok(Variable::Bool(lhs.resolve(scope)? != rhs.resolve(scope)?));
            },
            Token::Not(exp) => {
                if let Variable::Bool(exp) = exp.resolve(scope)? {
                    return Ok(Variable::Bool(!exp));
                } else {
                    return Err(Error::InvalidOperatorApplication("'!' on non-bool".to_string()));
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Scope<'s> {
    pub stack_offset: usize,
    pub stack_len: usize,
    pub locals: Locals,
    pub heap: Rc<RefCell<HashMap<usize, Variable>>>,
    pub stack: Rc<RefCell<Variables>>,
    pub functions: Rc<RefCell<Functions>>,
    pub instructions: &'s Vec<Instruction>,
    pub propagate_return_value: bool,
}

impl<'s> Scope<'s> {
    pub fn new(
        heap: Rc<RefCell<HashMap<usize, Variable>>>,
        stack: Rc<RefCell<Variables>>,
        functions: Rc<RefCell<Functions>>, 
        instructions: &'s Vec<Instruction>
    ) -> Self {
        Self {
            stack_offset: 0,
            stack_len: 0,
            locals: Locals::new(),
            heap,
            stack,
            functions, 
            instructions,
            propagate_return_value: false,
        }
    }

    pub fn child(&'s self, instructions: &'s Vec<Instruction>) -> Scope<'s> 
    {
        Self {
            stack_offset: self.stack.borrow().len() - 1,
            stack_len: 0,
            locals: self.locals.clone(),
            heap: self.heap.clone(),
            stack: self.stack.clone(),
            functions: self.functions.clone(),
            instructions,
            propagate_return_value: false,
        }
    }

    pub fn get_instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }

    pub fn add_hidden_variable(&mut self, value: Variable) -> usize {
        let index = self.stack.borrow().len();
        self.stack.borrow_mut().push(value);
        self.stack_len += 1;
        index
    }

    pub fn add_variable<S>(&mut self, key: S, value: Variable) -> usize
        where S: Into<String>
    {
        let index = self.stack.borrow().len();
        self.locals.insert(key.into(), index);
        self.stack.borrow_mut().push(value);
        self.stack_len += 1;
        index
    }

    pub fn add_function<S: Into<String>>(&mut self, ident: S, function: Function) {
        self.functions.borrow_mut().insert(ident.into(), function);
    }

    pub fn free(&mut self) {
        for _ in 0..self.stack_len {
            self.stack.borrow_mut().pop().unwrap();
        }
    }

    pub fn run(&mut self) -> Result<(Variable, usize), Error> {
        for token in self.instructions.clone().iter() {
            match token {
                Instruction::InitVariable(key, token_tree) => {
                    let index = self.stack.borrow().len();
                    self.stack.borrow_mut().push(Variable::Null);
                    
                    let value = token_tree.resolve(self)?;

                    self.stack.borrow_mut()[index] = value;
                    self.locals.insert(key.clone(), index);
                    self.stack_len += 1;
                },
                Instruction::SetVariable(target_tree, token_tree) => {
                    let value = token_tree.resolve(self)?;
                    let target = target_tree.resolve(self)?;
                    
                    if let Variable::Ref(reference) = target {
                        match reference {
                            Ref::Local(target) => {
                                let global = self.locals.get(&target).unwrap();

                                self.stack.borrow_mut()[*global] = value
                            },
                            Ref::Global(target) => self.stack.borrow_mut()[target] = value,
                            Ref::Heap(target) => *self.heap.borrow_mut().get_mut(&target).unwrap() = value,
                        }
                    } else {
                        return Err(Error::NullReference);
                    }
                },
                Instruction::Return(token_tree, len) => {
                    let location = self.stack.borrow().len();
                    let return_value = token_tree.resolve(self)?;

                    let return_values: Vec<Variable> = self.stack.borrow()[location..location+len-1]
                                                          .iter()
                                                          .map(|v| v.clone())
                                                          .collect();

                    self.free();

                    for v in return_values {
                        self.stack.borrow_mut().push(v);
                    }

                    return Ok((return_value, len-1));
                },  
                Instruction::If(check, instructions) => {
                    if let Variable::Bool(check) = check.resolve(self)? {
                        if check {
                            let mut child_scope = self.child(&instructions);

                            child_scope.run()?;
                        }
                    } else {
                        return Err(Error::InvalidIf("token tree in if statement resolved to a non-bool"));
                    }
                },
                Instruction::FunctionCall(function_call) => {
                    function_call.resolve(self)?;
                },
                Instruction::AddFunction(ident, function) => {
                    self.functions.borrow_mut().insert(ident.clone(), function.clone());
                }
            }
        }

        self.free();

        Ok((Variable::Null, 0))
    }
}

pub struct Interpreter {
    pub functions: HashMap<String, (fn(Vec<Variable>) -> Variable, parser::Function)>, 
}

impl Interpreter {
    pub fn add_function<S: Into<String>>(&mut self, ident: S, function: (fn(Vec<Variable>) -> Variable, parser::Function)) {
        self.functions.insert(ident.into(), function);
    }

    pub fn new() -> Self
    {      
        Interpreter {
            functions: HashMap::new(),
        }
    }
    
    pub fn run<S>(&mut self, code: S) -> Result<Variable, error::Error> 
        where S: Into<String>
    {
        let mut functions = HashMap::new();
        let mut scope_functions = HashMap::new();

        for (ident, (function, parameters)) in &self.functions {
            functions.insert(ident.clone(), parameters.clone());
            scope_functions.insert(ident.clone(), Function::Rust(*function, parameters.parameters.clone()));
        }

        let heap = Rc::new(RefCell::new(HashMap::new()));
        let stack = Rc::new(RefCell::new(Variables::new()));
        let instructions = crate::language::parse(&mut code.into().trim().to_string(), 
                                                 HashMap::new(), 
                                                 HashMap::new(), 
                                                 functions);

        if let Err(error) = instructions {
            return Err(error::Error::ParseError(error))
        }

        let instructions = instructions.unwrap();

        let mut scope = Scope::new(
            heap.clone(),
            stack.clone(),
            Rc::new(RefCell::new(scope_functions)),
            &instructions,
        );

        for i in &instructions {
            println!("INSTRUCTION: {:?}", i);
        }

        println!("\nOUTPUT:");

        let return_value = match scope.run() {
            Ok((v, _)) => Ok(v),
            Err(e) => Err(error::Error::InterpreterError(e)) 
        };

        println!("");

        for (i, v) in scope.stack.borrow().iter().enumerate() {
            println!("STACKLEAK_{}: {:?}", i, v);
        }
        
        return_value
    }
}
