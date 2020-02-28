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
    ($f:ident($($arg:expr),*) -> $rty:expr) => {
        (
            $f, 
            language::parser::Function { 
                parameters: vec![$(Var { var_type: $arg, len: 1 }),*], 
                return_value: language::Var { 
                    var_type: $rty, 
                    len: 1 
                } 
            }
        )
    };
}

type Locals = std::collections::HashMap<String, usize>;
type Variables = Vec<Variable>;
type Functions = std::collections::HashMap<String, Function>;

#[derive(Clone)]
pub enum Function {
    Rust(fn(&mut Scope, Vec<Variable>) -> Variable, Vec<Var>),
    Native(Vec<Instruction>, Vec<(String, Var)>),
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Function::Native(instructions, _) = self {
            let _ = f.write_str(format!("{:?}", instructions).as_str());
        }
        
        Ok(())
    }
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

                let output = function(&mut scope, params.clone());

                scope.free();

                Ok((output, 0))
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
                        value = Variable::Ref(Ref::Global(*scope.locals.get(&reference).expect("local entry doesnt exist")));
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
    While(Token, Vec<Instruction>),
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
    InitSlice(Box<Token>, Box<Token>, usize),
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
                        Ref::Local(reference) => *scope.locals.get(&reference).expect("heap entry doesnt exist"),
                        Ref::Global(reference) => reference,
                        Ref::Heap(location) => {
                            if let Variable::Slice(location, len, element_len) = 
                                scope.heap.borrow().get(&location).expect("heap entry doesnt exist")
                            {
                                let mut new_location = 0;
     
                                let mut heap = scope.heap.borrow_mut();
                                let mut peekable = heap.iter().collect::<Vec<_>>();
                                peekable.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());
                                let mut peekable = peekable.iter().peekable();

                                while let Some((location, _)) = peekable.next() {
                                    if let Some((next_location, _)) = peekable.peek() {
                                        if **next_location - **location >= *len {
                                            new_location = **next_location - **location;
                                            break;
                                        }
                                    } else {
                                        new_location = **location;
                                        break;
                                    }
                                }                                 
     
                                for i in 0..len * element_len {
                                    let variable = heap.get(&(location + i)).expect("heap entry doesnt exist").clone();
                                    heap.insert(new_location + i, variable);
                                }
     
                                return Ok(Variable::Slice(new_location, *len, *element_len));
                            }
                             
                            let mut new_location = 0;

                            let heap = scope.heap.borrow();
                            let mut peekable = heap.iter().collect::<Vec<_>>();
                            peekable.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());
                            let mut peekable = peekable.iter().peekable();

                            while let Some((location, _)) = peekable.next() {
                                if let Some((next_location, _)) = peekable.peek() {
                                    if **next_location - **location >= *len {
                                        new_location = **next_location - **location;
                                        break;
                                    }
                                } else {
                                    new_location = **location;
                                    break;
                                }
                            }

                            for i in 1..*len { 
                                let variable = scope.heap.borrow().get(&(location + i)).expect("heap entry doesnt exist").clone();
                                scope.heap.borrow_mut().insert(new_location + i, variable);
                            }
     
                            return Ok(scope.heap.borrow().get(&location).expect("heap entry doesnt exist").clone());
                        }
                    };

                    if let Variable::Slice(location, len, element_len) = scope.stack.borrow()[location] {
                        let mut new_location = 0;

                        let mut heap = scope.heap.borrow_mut();
                        let mut peekable = heap.iter().collect::<Vec<_>>();
                        peekable.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());
                        let mut peekable = peekable.iter().peekable();

                        while let Some((location, _)) = peekable.next() {
                            if let Some((next_location, _)) = peekable.peek() {
                                if **next_location - **location >= len as usize * element_len {
                                    new_location = **next_location - **location;
                                    break;
                                }
                            } else {
                                new_location = **location;
                                break;
                            }
                        }
                            

                        for i in 0..len * element_len {
                            let variable = heap.get(&(location + i)).expect("heap entry doesnt exist").clone();
                            heap.insert(new_location + i, variable);
                        }

                        return Ok(Variable::Slice(new_location, len, element_len));
                    }

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
                                (&mut stack[*scope.locals.get(&reference).expect("local doesnt exist")], 
                                 *scope.locals.get(&reference).expect("local doesnt exist"))
                            },
                            Ref::Global(reference) => {
                                (&mut stack[reference], reference)
                            },
                            Ref::Heap(reference) => {
                                let object = heap.get_mut(&reference).expect("no heap entry");

                                if let Variable::Struct(object) = object {
                                    if let Some(field) = object.get(field) {
                                        let field = field.clone();
     
                                        return Ok(Variable::Ref(Ref::Heap(reference + field)));
                                    } else {
                                        return Err(Error::IndexError("Trying to access non-field"));
                                    }
                                } else {
                                    return Err(Error::IndexError("Trying to access field on non-struct"));
                                }
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
                            (&mut stack[*scope.locals.get(&reference).expect("local doesnt exist")], 
                             *scope.locals.get(&reference).expect("local doenst exist"))
                        },
                        Ref::Global(reference) => {
                            (&mut stack[reference], reference)
                        },
                        Ref::Heap(reference) => {
                            (heap.get_mut(&reference).expect("heap entry doesnt exist"), reference)
                        },
                    };

                    if let Variable::Array(_) = array {
                        if let Variable::Int(index) = index {
                            return Ok(Variable::Ref(Ref::Global(location + 1 + index as usize * *element_len)));
                        } else {
                            return Err(Error::IndexError("Arrays are only indexed by integers"));
                        }
                    } else if let Variable::Slice(location, _, _) = array {
                        if let Variable::Int(index) = index {
                            return Ok(Variable::Ref(Ref::Heap(*location + index as usize * *element_len)));
                        } else {
                            return Err(Error::IndexError("Slices are only indexed by integers"));
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
            Token::InitSlice(element, len, element_len) => {
                if let Variable::Int(len) = len.resolve(scope)? {
                    let mut new_location = 0;

                    let heap = scope.heap.borrow();
                    let mut peekable = heap.iter().collect::<Vec<_>>();
                    peekable.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());
                    let mut peekable = peekable.iter().peekable();

                    while let Some((location, _)) = peekable.next() {
                        if let Some((next_location, _)) = peekable.peek() {
                            if **next_location - **location >= len as usize * *element_len {
                                new_location = **next_location - **location;
                                break;
                            }
                        } else {
                            new_location = **location;
                            break;
                        }
                    }

                    drop(heap);
                    
                    let index = scope.stack.borrow().len();
                    let variable = element.resolve(scope)?;

                    for i in 0..len as usize {  
                        scope.heap.borrow_mut().insert(new_location + i * element_len, variable.clone());

                        for j in 1..*element_len {
                            let variable = scope.stack.borrow()[index + j].clone();
                            scope.heap.borrow_mut().insert(new_location + i * element_len + j, variable);
                        }
                    }

                    return Ok(Variable::Slice(new_location, len as usize, *element_len));
                } else {
                    return Err(Error::NullReference);
                }
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
                let lhs = lhs.resolve(scope)?;
                let rhs = rhs.resolve(scope)?;

                let output = Variable::Bool(lhs == rhs);

                lhs.free(scope);
                rhs.free(scope);

                return Ok(output);
            },
            Token::Neq(lhs, rhs) => {
                let lhs = lhs.resolve(scope)?;
                let rhs = rhs.resolve(scope)?;

                let output = Variable::Bool(lhs != rhs);

                lhs.free(scope);
                rhs.free(scope);

                return Ok(output);            
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

pub struct Scope<'s> {
    pub stack_offset: usize,
    pub stack_len: usize,
    pub locals: Locals,
    pub heap: Rc<RefCell<HashMap<usize, Variable>>>,
    pub stack: Rc<RefCell<Variables>>,
    pub functions: Rc<RefCell<Functions>>,
    pub instructions: &'s Vec<Instruction>,
    pub propagate_return_value: bool,
    pub script_data: std::sync::Arc<std::sync::Mutex<crate::states::ScriptData>>,
}

impl<'s> Scope<'s> {
    pub fn new(
        heap: Rc<RefCell<HashMap<usize, Variable>>>,
        stack: Rc<RefCell<Variables>>,
        functions: Rc<RefCell<Functions>>, 
        instructions: &'s Vec<Instruction>,
        script_data: std::sync::Arc<std::sync::Mutex<crate::states::ScriptData>>,
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
            script_data
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
            script_data: self.script_data.clone()
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
            let x = self.stack.borrow_mut().pop().expect("pop failed");
            x.free(self);
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
                                let global = self.locals.get(&target).expect("local doesnt exist");

                                self.stack.borrow_mut()[*global] = value
                            },
                            Ref::Global(target) => self.stack.borrow_mut()[target] = value,
                            Ref::Heap(target) => *self.heap.borrow_mut().get_mut(&target).expect("value doesnt exist on heap") = value,
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

                            let return_value = child_scope.run()?;

                            if return_value != (Variable::Null, 1) {
                                return Ok(return_value);
                            }
                        }
                    } else {
                        return Err(Error::InvalidIf("token tree in if statement resolved to a non-bool"));
                    }
                },
                Instruction::While(check, instructions) => {
                    loop {
                        if let Variable::Bool(check) = check.resolve(self)? {
                            if check {
                                let mut child_scope = self.child(&instructions);

                                let return_value = child_scope.run()?;

                                if return_value != (Variable::Null, 1) {
                                    return Ok(return_value);
                                }
                            } else {
                                break;
                            }
                        } else {
                            return Err(Error::InvalidIf("token tree in while statement resolved to a non-bool"));
                        }
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

        Ok((Variable::Null, 1))
    }
}

pub struct Interpreter {
    pub functions: HashMap<String, (fn(&mut Scope, Vec<Variable>) -> Variable, parser::Function)>, 
    pub structs: HashMap<String, Struct>,
    pub instructions: Vec<Instruction>
}

impl Interpreter {
    pub fn add_function<S: Into<String>>(
        &mut self, 
        ident: S, 
        function: (fn(&mut Scope, Vec<Variable>) -> Variable, parser::Function)
    ) {
        self.functions.insert(ident.into(), function);
    }

    pub fn add_struct<S: Into<String>>(
        &mut self, 
        ident: S, 
        _struct: Struct
    ) {
        self.structs.insert(ident.into(), _struct);
    }


    pub fn new() -> Self
    {      
        let mut interpreter = Interpreter {
            functions: HashMap::new(),
            structs: HashMap::new(),
            instructions: Vec::new(),
        };

        fn print(_: &mut language::Scope, variables: Vec<language::Variable>) -> language::Variable {
            println!("{:?}", variables[0]);

            language::Variable::Null
        }

        fn len(_: &mut Scope, variables: Vec<Variable>) -> Variable {
            if let Variable::Slice(_, len, _) = variables[0] {
                Variable::Int(len as i32)
            } else {
                Variable::Int(1)
            }
        }

        use crate::language;
        interpreter.add_function("print", function!(print(VarType::Any) -> VarType::Null));
        interpreter.add_function("len", function!(len(VarType::Any) -> VarType::Int));

        interpreter
    }
    
    pub fn run<S>(
        &mut self, 
        code: S, 
        script_data: std::sync::Arc<std::sync::Mutex<crate::states::ScriptData>>
    ) -> Result<Variable, error::Error> 
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
                                                  self.structs.clone(), 
                                                  HashMap::new(), 
                                                  functions);

        if let Err(error) = instructions {
            return Err(error::Error::ParseError(error))
        }

        let instructions = instructions.expect("wut?");

        let mut scope = Scope::new(
            heap.clone(),
            stack.clone(),
            Rc::new(RefCell::new(scope_functions)),
            &instructions,
            script_data,
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
        
        for (i, v) in scope.heap.borrow().iter().enumerate() {
            println!("HEAPLEAK_{}: {:?}", i, v);
        }
        
        return_value
    }
}
