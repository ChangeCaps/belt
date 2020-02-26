use crate::language::*;
use std::collections::HashMap;

pub const VAR_IDENTS: [(&'static str, Var); 4] = [
    ("int", Var { var_type: VarType::Int, len: 1 }),
    ("float", Var { var_type: VarType::Float, len: 1 }),
    ("bool", Var { var_type: VarType::Bool, len: 1 }),
    ("string", Var { var_type: VarType::String, len: 1 }),
];

#[derive(Clone, Debug, PartialEq)]
pub enum VarType {
    Int,
    Float,
    Bool,
    FunctionRef,
    Ref(Box<Var>),
    String,
    Slice(Box<Var>),
    Array(Box<Var>, usize),
    Struct(String),
    Null,
    Any,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub var_type: VarType,
    pub len: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub len: usize,
    pub fields: HashMap<String, Var>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub parameters: Vec<Var>,
    pub return_value: Var,
}

pub fn parse(
    code: &mut String, 
    mut structs: HashMap<String, Struct>, 
    mut vars: HashMap<String, Var>, 
    mut functions: HashMap<String, Function>
) -> Result<Vec<Instruction>, ParseError> {
    let mut instructions = Vec::new();

    while code.len() > 0 {
        let mut lexer = Lexer::new(code);

        if lexer.find("let") {
            lexer.skip_space();
            
            let target = lexer.get(" ");
            lexer.skip_space();
            
            if !lexer.find("=") {
                return Err(ParseError::MissingToken("="));
            }

            lexer.skip_space();
            let token_tree = lexer.get_extensive_skip("[", "]", ";");
            
            if !lexer.find(";") {
                return Err(ParseError::MissingToken(";"));
            }

            if target.is_some() && token_tree.is_some() {
                let (token, var) = parse_token_tree(token_tree.unwrap(), &structs, &vars, &functions)?;

                instructions.push(
                    Instruction::InitVariable(
                        target.clone().unwrap(),
                        token
                    )
                );

                vars.insert(target.unwrap(), var);

                lexer.confirm();
                continue;
            }
        }

        if lexer.find("if") {
            if lexer.skip_space() {
                if let Some(check) = lexer.get("{") {
                    lexer.find("{");
                    if let Some(mut scope) = lexer.get_expansive("{", "}") {
                        lexer.find("}");

                        let (token, var) = parse_token_tree(check, &structs, &vars, &functions)?;

                        if var.var_type == VarType::Bool {
                            instructions.push(
                                Instruction::If(
                                    token,
                                    parse(&mut scope, structs.clone(), vars.clone(), functions.clone())?
                                )
                            );
                            lexer.confirm();
                            continue;
                        } else {
                            return Err(ParseError::TypeMismatch(format!("Tried to check '{:?}'", var.var_type).to_string()))
                        }
                    } else {
                        return Err(ParseError::MissingToken("'}' in if statement"));
                    }
                } else {
                    return Err(ParseError::MissingToken("'{' in if statement"));
                }
            }
        }

        lexer.restart();

        // look for pattern return#;
        if lexer.find("return") {
            if lexer.skip_space() {
                if lexer.find(";") {
                    instructions.push(Instruction::Return(Token::Variable(Variable::Null), 1));
                    lexer.confirm();
                    continue;
                }

                if let Some(token_tree) = lexer.get(";") {
                    let (token, var) = parse_token_tree(token_tree, &structs, &vars, &functions)?;

                    instructions.push(Instruction::Return(token, var.len));
                    lexer.find(";");
                    lexer.confirm();
                    continue;
                }
            } else if lexer.find(";") {
                instructions.push(Instruction::Return(Token::Variable(Variable::Null), 1));
                lexer.confirm();
                continue;
            }
        }

        lexer.restart();

        // look for pattern: "#"
        if let Some(token) = lexer.get(" ") {
            // look for pattern "# = #;"
            lexer.skip_space();
                
            if lexer.find("=") {
                lexer.skip_space();
                let token_tree = lexer.get_extensive_skip("[", "]", ";");

                // insure we have a semicolon at the end of a line
                if !lexer.find(";") {
                    return Err(ParseError::MissingToken(";"));
                }

                if token_tree.is_some() {
                    let (mut target, target_var) = parse_token_tree(token, &structs, &vars, &functions)?;
                    let (value, value_var) = parse_token_tree(token_tree.unwrap(), &structs, &vars, &functions)?;

                    if target_var != value_var {
                        return Err(ParseError::TypeMismatch(format!("Tried to set '{:?}' = '{:?}'", target_var.var_type, value_var.var_type).to_string()));
                    }

                    // remove deref
                    if let Token::Deref(token_tree, _) = target {
                        target = *token_tree;
                    }

                    // add instruction
                    instructions.push(
                        Instruction::SetVariable(
                            target,
                            value
                        )
                    );

                    lexer.confirm();
                    continue;
                }
            }
        }

        lexer.restart();

        if lexer.find("struct") {
            if lexer.skip_space() {
                if let Some(struct_ident) = lexer.get("{") {
                    let struct_ident = struct_ident.trim().to_string();

                    lexer.find("{");

                    if !parse_reference(&struct_ident) {
                        return Err(ParseError::VariableParseFailiure(struct_ident));
                    }

                    if let Some(mut fields) = lexer.get_expansive("{", "}") {
                        lexer.find("}");

                        let mut fields_lexer = Lexer::new(&mut fields);

                        let mut fields = HashMap::new();

                        while let Some(field) = fields_lexer.get(",") {
                            fields_lexer.find(",");
                            fields_lexer.confirm();

                            let (ident, var) = parse_type_declaration(field, &structs)?;

                            fields.insert(ident, var);
                        }

                        if fields_lexer.get_code().1.trim().len() > 0 {
                            let (ident, var) = parse_type_declaration(fields_lexer.get_code().1.to_string(), &structs)?;

                            fields.insert(ident, var);
                        }

                        let mut len = 1;

                        for (_, field) in &fields {
                            len += field.len;
                        }

                        structs.insert(
                            struct_ident,
                            Struct {
                                fields,
                                len,
                            }
                        );

                        lexer.confirm();
                        continue;
                    } else {
                        return Err(ParseError::MissingToken("}"));
                    }
                } else {
                    return Err(ParseError::MissingToken("{"));
                }
            }
        }

        lexer.restart();

        if lexer.find("fn") {
            if lexer.skip_space() {
                if let Some(function_ident) = lexer.get("(") {
                    lexer.find("(");
                    if let Some(parameters) = lexer.get(")") {
                        lexer.find(")");
                        let mut params = Vec::new();
                        let mut vars = HashMap::new();

                        let mut parameters = parameters.to_string();

                        let mut parameter_lexer = Lexer::new(&mut parameters);

                        
                        while let Some(parameter) = parameter_lexer.get(",") {
                            parameter_lexer.find(",");
                            
                            let (ident, ty) = parse_type_declaration(parameter.to_string(), &structs)?;

                            vars.insert(ident.clone(), ty.clone());
                            params.push((ident, ty));
                        }
                        
                        let parameter_code = parameter_lexer.get_code().1.to_string();
                        
                        if parameter_code.trim().len() > 0 {
                            let (ident, ty) = parse_type_declaration(parameter_lexer.get_code().1.to_string(), &structs)?;

                            vars.insert(ident.clone(), ty.clone());
                            params.push((ident, ty));
                        }
                        
                        if !parse_reference(&function_ident) {
                            return Err(ParseError::InvalidFunctionIdent(function_ident));
                        }
                        
                        lexer.skip_space();
                        
                        let return_type = if lexer.find("->") {
                            if let Some(return_type) = lexer.get("{") {
                                let return_type = return_type.trim().to_string();

                                if parse_reference(&return_type) {
                                    if let Ok(ty) = parse_type(return_type, &structs) {
                                        ty
                                    } else {
                                        Var { var_type: VarType::Null, len: 1 }
                                    }
                                } else {
                                    Var { var_type: VarType::Null, len: 1 }
                                }
                            } else {
                                Var { var_type: VarType::Null, len: 1 }
                            }
                        } else {
                            Var { var_type: VarType::Null, len: 1 }
                        };

                        if lexer.find("{") {
                            if let Some(mut scope) = lexer.get_expansive("{", "}") {
                                lexer.find("}");

                                functions.insert(
                                    function_ident.clone(),
                                    Function {
                                        parameters: params.clone().into_iter().map(|(_, ty)| ty).collect(),
                                        return_value: return_type,
                                    }
                                );

                                instructions.push(Instruction::AddFunction(
                                    function_ident, 
                                    interpreter::Function::Native(
                                        parse(&mut scope, structs.clone(), vars, functions.clone())?, 
                                        params
                                    )
                                )); 

                                lexer.confirm();
                                continue;
                            } else {
                                return Err(ParseError::MissingToken("}"));
                            }
                        } else {
                            return Err(ParseError::MissingToken("{"));
                        }
                    } else {
                        return Err(ParseError::MissingToken(")"));
                    }
                } else {
                    return Err(ParseError::MissingToken("("));
                }
            }
        }

        let mut lexer = Lexer::new(code);

        // tries to look for naked function calls
        if let Some(token) = lexer.get_extensive_skip("[", "]", ";") {
            lexer.find(";");

            if let Some(funcion_call) = parse_funtion_call(&mut token.clone(), &structs, &vars, &functions) {
                instructions.push(Instruction::FunctionCall(funcion_call?.0));

                lexer.confirm();

                continue;
            }
        }

        //return Err(ParseError::UnexpectedToken(code.to_string()));
    }

    Ok(instructions)
}

pub fn parse_field(
    mut code: String, 
    structs: &HashMap<String, Struct>, 
    vars: &HashMap<String, Var>,
    functions: &HashMap<String, Function>,
) -> Result<(String, (Token, Var)), ParseError> {
    let mut lexer = Lexer::new(&mut code);

    if let Some(field_ident) = lexer.get(":") {
        lexer.find(":");
        lexer.confirm();

        let field_ident = field_ident.trim().to_string();
        let code = code.trim().to_string();
        
        if parse_reference(&field_ident) {
            return Ok((field_ident, parse_token_tree(code, structs, vars, functions)?));
        }
    }

    Err(ParseError::VariableParseFailiure(code))
}

pub fn parse_type(mut code: String, structs: &HashMap<String, Struct>) -> Result<Var, ParseError> {
    let mut lexer = Lexer::new(&mut code);

    if lexer.find("&[") {
        if let Some(ty) = lexer.get_expansive("[", "]") {
            let var = parse_type(ty, structs)?;

            lexer.confirm();
            return Ok(Var {
                len: 1,
                var_type: VarType::Slice(Box::new(var)),
            });
        } else {
            return Err(ParseError::MissingToken("]"));
        }
    }

    if lexer.find("&") {
        lexer.confirm();
        return Ok(Var { 
            var_type: VarType::Ref(Box::new(parse_type(code, structs)?)),
            len: 1
        });
    }

    if lexer.find("[") {
        lexer.skip_space();
        if let Some(mut ty) = lexer.get_expansive("[", "]") {
            let mut lexer = Lexer::new(&mut ty);

            if let Some(ty) = lexer.get_extensive_skip("[", "]", ";") {
                lexer.find(";");
                lexer.skip_space();

                let len = match lexer.get_code().1.trim().parse::<usize>() {
                    Ok(len) => len,
                    Err(_) => return Err(
                        ParseError::VariableParseFailiure(lexer.get_code().1.trim().to_string())
                    )
                };

                let var = parse_type(ty, structs)?;
                
                return Ok(Var { 
                    len: len * var.len + 1,
                    var_type: VarType::Array(Box::new(var), len),
                });
            } else {
                return Ok(Var { var_type: VarType::Slice(Box::new(parse_type(ty, structs)?)), len: 1 });
            }
        } else {
            return Err(ParseError::MissingToken("]"));
        }
    }

    if let Some(s) = structs.get(&code) {
        return Ok(Var { var_type: VarType::Struct(code), len: s.len });
    } else {
        for (ident, ty) in &VAR_IDENTS {
            if code.as_str() == *ident {
                return Ok(ty.clone());
            }
        } 
    }

    return Err(ParseError::VariableParseFailiure(code));
}

pub fn parse_type_declaration(
    mut code: String, 
    structs: &HashMap<String, Struct>
) -> Result<(String, Var), ParseError> {
    let mut lexer = Lexer::new(&mut code);

    if let Some(field_ident) = lexer.get(":") {
        lexer.find(":");
        lexer.confirm();

        let field_ident = field_ident.trim().to_string();
        let code = code.trim().to_string();
        
        let ty = parse_type(code, structs)?;

        return Ok((field_ident, ty));
    }

    Err(ParseError::VariableParseFailiure(code))
}

// checks whether the given token is a valid variable name
pub fn parse_reference(string: &String) -> bool {
    const MUST_CONTAIN: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const VALID_CHARACTERS: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";

    let contains = string.find(|c: char| MUST_CONTAIN.contains(c)).is_some();

    string.find(|c: char| !VALID_CHARACTERS.contains(c)).is_none() && string.len() > 0 && contains
}


// tries to parse the given code as a function call
pub fn parse_funtion_call(
    code: &mut String, 
    structs: &HashMap<String, Struct>, 
    vars: &HashMap<String, Var>, 
    functions: &HashMap<String, Function>
) -> Option<Result<(Token, Var), ParseError>> {
    let mut lexer = Lexer::new(code);

    if let Some(function_ident) = lexer.get("(") {
        lexer.find("(");

        if !parse_reference(&function_ident) {
            return None;
        }

        if let Some(token) = lexer.get_expansive("(", ")") {
            lexer.find(")");

            if let Some(function) = functions.get(&function_ident) {
                let mut token = token.clone();

                let mut params_lexer = Lexer::new(&mut token);

                let mut parameters = Vec::new();

                let mut param_index = 0;

                while let Some(parameter) = params_lexer.get_extensive_skip("(", ")", ",") {
                    params_lexer.find(",");

                    if function.parameters.len() > param_index {
                        let (tree, var) = match parse_token_tree(parameter, structs, vars, functions) {
                            Ok(x) => x,
                            Err(err) => return Some(Err(err)),
                        };

                        if var != function.parameters[param_index] && function.parameters[param_index].var_type != VarType::Any {
                            return Some(
                                Err(
                                    ParseError::TypeMismatch(
                                        format!(
                                            "Parameter '{}' on '{}' is supposed to be '{:?}', not {:?}", 
                                            param_index, 
                                            function_ident, 
                                            function.parameters[param_index].var_type,
                                            var.var_type
                                        ).to_string()
                                    )
                                )
                            )
                        }

                        parameters.push(tree);
                    } else {
                        return Some(Err(ParseError::TooFewParameters));
                    }

                    param_index += 1;
                }
                
                params_lexer.confirm();
                
                if token.trim().len() > 0 {
                    if function.parameters.len() > param_index {
                        let (tree, var) = match parse_token_tree(token, structs, vars, functions) {
                            Ok(x) => x,
                            Err(err) => return Some(Err(err)),
                        };

                        if var != function.parameters[param_index] && function.parameters[param_index].var_type != VarType::Any {
                            return Some(
                                Err(
                                    ParseError::TypeMismatch(
                                        format!(
                                            "Parameter '{}' on '{}' is supposed to be '{:?}' not '{:?}'", 
                                            param_index, 
                                            function_ident, 
                                            function.parameters[param_index].var_type,
                                            var.var_type
                                        ).to_string()
                                    )
                                )
                            )
                        }

                        parameters.push(tree);
                    } else {
                        return Some(Err(ParseError::TooFewParameters));
                    }

                    param_index += 1;
                }
                
                if param_index > function.parameters.len() {
                    return Some(Err(ParseError::TooManyParameters));
                }

                lexer.confirm();

                return Some(Ok((
                    Token::FunctionCall(
                        Box::new(
                            Token::Variable(
                                Variable::FunctionRef(
                                    function_ident
                                )
                            )
                        ), 
                        parameters
                    ),
                    function.return_value.clone()        
                )));
            } else {
                return Some(Err(ParseError::UnrecognizedFunction(function_ident)));
            }
        } else {
            return Some(Err(ParseError::MissingToken(")")));
        }
    }

    return None;
}

// a function for parsing token trees, this is done recursively, and through each iteration goes one layer further down.
pub fn parse_token_tree(
    mut string: String, 
    structs: &HashMap<String, Struct>, 
    vars: &HashMap<String, Var>,
    functions: &HashMap<String, Function>,
) -> Result<(Token, Var), ParseError> {
    // function calls
    if let Some(function_call) = parse_funtion_call(&mut string, structs, vars, functions) {
        return function_call;
    } 

    let mut lexer = Lexer::new(&mut string);

    if let Some(struct_ident) = lexer.get("{") {
        let struct_ident = struct_ident.trim().to_string();

        lexer.find("{");

        if !parse_reference(&struct_ident) {
            return Err(ParseError::VariableParseFailiure(struct_ident));
        }

        if let Some(mut fields) = lexer.get_expansive("{", "}") {
            lexer.find("}");

            if let Some(s) = structs.get(&struct_ident) {
                let mut fields_lexer = Lexer::new(&mut fields);

                let mut fields = Vec::new();

                while let Some(field) = fields_lexer.get_extensive_skip("{", "}", ",") {
                    fields_lexer.find(",");
                    fields_lexer.confirm();

                    let (field_ident, (token_tree, var)) = parse_field(field, &structs, vars, functions)?;

                    if let Some(field) = s.fields.get(&field_ident) {
                        if *field == var {
                            fields.push((field_ident, token_tree));
                        } else {
                            return Err(ParseError::TypeMismatch(format!("{:?} and {:?}", field, var).to_string()));
                        }
                    } else {
                        return Err(ParseError::UnrecognizedField(struct_ident, field_ident));
                    }
                }          
                
                if fields_lexer.get_code().1.trim().len() > 0 {
                    let (field_ident, (token_tree, var)) = parse_field(fields_lexer.get_code().1.to_string(), 
                                                                       &structs, 
                                                                       vars,
                                                                       functions)?;

                    if let Some(field) = s.fields.get(&field_ident) {
                        if *field == var {
                            fields.push((field_ident, token_tree));
                        } else {
                            return Err(ParseError::TypeMismatch(format!("{:?} and {:?}", field, var).to_string()));
                        }
                    } else {
                        return Err(ParseError::UnrecognizedField(struct_ident, field_ident));
                    }
                }

                lexer.confirm();
                return Ok((Token::InitStruct(fields), Var { var_type: VarType::Struct(struct_ident), len: s.len }));
            }
        } else {
            return Err(ParseError::MissingToken("}"));
        }
    }

    // arithmatic
    if let Some(token) = lexer.get("+") {
        lexer.find("+");
        lexer.confirm();

        let lhs = parse_token_tree(token, structs, vars, functions)?;
        let rhs = parse_token_tree(string, structs, vars, functions)?;

        if (lhs.1.var_type == VarType::Int && rhs.1.var_type == VarType::Int) || 
           (lhs.1.var_type == VarType::Float && rhs.1.var_type == VarType::Float) {
            return Ok((Token::Add(Box::new(lhs.0), Box::new(rhs.0)), lhs.1));
        } else {
            return Err(ParseError::TypeMismatch(format!("Tried to add '{:?}' and '{:?}'", lhs.1.var_type, rhs.1.var_type).to_string()));
        }
    }
    if let Some(token) = lexer.get("-") {
        lexer.find("-");
        
        let lhs = parse_token_tree(token, structs, vars, functions);
        
        if lhs.is_ok() {
            lexer.confirm();

            let lhs = lhs.unwrap();
            let rhs = parse_token_tree(string, structs, vars, functions)?;

            if (lhs.1.var_type == VarType::Int && rhs.1.var_type == VarType::Int) || 
            (lhs.1.var_type == VarType::Float && rhs.1.var_type == VarType::Float) {
                return Ok((Token::Sub(Box::new(lhs.0), Box::new(rhs.0)), lhs.1));
            } else {
                return Err(ParseError::TypeMismatch(format!("Tried to subtract '{:?}' and '{:?}'", lhs.1.var_type, rhs.1.var_type).to_string()));
            }
        }
    }
    lexer.restart();
    while let Some(_) = lexer.get("*") {
        let lhs = parse_token_tree(lexer.get_code().0.to_string(), structs, vars, functions);
        
        lexer.find("*");
        
        let rhs = parse_token_tree(lexer.get_code().1.to_string(), structs, vars, functions);
        
        if lhs.is_ok() && rhs.is_ok() {
            let lhs = lhs.unwrap();
            let rhs = rhs.unwrap();

            if (lhs.1.var_type == VarType::Int && rhs.1.var_type == VarType::Int) || 
               (lhs.1.var_type == VarType::Float && rhs.1.var_type == VarType::Float) {
                lexer.confirm();

                return Ok((Token::Mul(Box::new(lhs.0), Box::new(rhs.0)), lhs.1));
            } else {
                return Err(ParseError::TypeMismatch(format!("Tried to multiply '{:?}' and '{:?}'", lhs.1.var_type, rhs.1.var_type).to_string()));
            }
        }
    }
    lexer.restart();
    if let Some(token) = lexer.get("/") {
        lexer.find("/");
        lexer.confirm();

        let lhs = parse_token_tree(token, structs, vars, functions)?;
        let rhs = parse_token_tree(string, structs, vars, functions)?;

        if (lhs.1.var_type == VarType::Int && rhs.1.var_type == VarType::Int) || 
           (lhs.1.var_type == VarType::Float && rhs.1.var_type == VarType::Float) {
            return Ok((Token::Div(Box::new(lhs.0), Box::new(rhs.0)), lhs.1));
        } else {
            return Err(ParseError::TypeMismatch(format!("Tried to devide '{:?}' and '{:?}'", lhs.1.var_type, rhs.1.var_type).to_string()));
        }
    }

    if let Some(token) = lexer.get("==") {
        lexer.find("==");
        lexer.confirm();

        let lhs = parse_token_tree(token, structs, vars, functions)?;
        let rhs = parse_token_tree(string, structs, vars, functions)?;

        if lhs.1 == rhs.1 {
            return Ok((Token::Eq(Box::new(lhs.0), Box::new(rhs.0)), Var { var_type: VarType::Bool, len: 1 }));
        } else {
            return Err(ParseError::TypeMismatch(format!("Tried to compare '{:?}' and '{:?}'", lhs.1.var_type, rhs.1.var_type).to_string()));
        }
    }

    if let Some(struct_ident) = lexer.rget(".") {
        lexer.find(".");

        let ident = lexer.get_code().1.to_string();
        let field_ident = lexer.get_any(vec![" ", ".", ",", "(", ")", "[", "]", "*", "/", "+", "-", "&"]);

        if field_ident.is_some() || parse_reference(&ident) {
            let field_ident = field_ident.unwrap_or(ident);

            lexer.skip(1);

            if !parse_reference(&field_ident) {
                return Err(ParseError::VariableParseFailiure(
                    format!("{} is not a valid field on {}", field_ident, struct_ident).to_string()
                ));
            }

            let (struct_token, struct_type) = parse_token_tree(struct_ident, structs, vars, functions)?;

            if let VarType::Struct(s) = &struct_type.var_type {
                if let Some(var) = structs.get(s).unwrap().fields.get(&field_ident) {
                    return Ok(
                        (
                            Token::Deref(Box::new(Token::AccessField(Box::new(struct_token), field_ident)), var.len),
                            var.clone()
                        )
                    );
                }
            } else {
                return Err(ParseError::InvalidOperator(format!("'.' on {:?}", struct_type.var_type).to_string()));
            }
        }
    }

    if lexer.find("&[") {
        if let Some(mut elements) = lexer.get_expansive("[", "]") {
            lexer.find("]");

            let mut lexer = Lexer::new(&mut elements);

            if let Some(element_type) = lexer.get_extensive_skip("[", "]", ";") {
                lexer.find(";");

                let (element, element_type) = parse_token_tree(element_type.clone(), 
                                                               structs,
                                                               vars,
                                                               functions)?;

                let len = parse_token_tree(lexer.get_code().1.to_string(),
                                           structs,
                                           vars,
                                           functions)?;

                let var = Var {
                    len: 1,
                    var_type: VarType::Slice(Box::new(element_type.clone())),
                };

                if len.1.var_type == VarType::Int {
                    lexer.confirm();
                    return Ok((Token::InitSlice(Box::new(element), Box::new(len.0), element_type.len), var));
                } else {
                    return Err(ParseError::VariableParseFailiure(lexer.get_code().1.to_string()));
                }
            }
        }
    }

    // ref & deref
    if lexer.find("*") {
        lexer.confirm();

        let exp = parse_token_tree(string, structs, vars, functions)?;

        if let VarType::Ref(reference) = exp.1.var_type {
            return Ok((Token::Deref(Box::new(exp.0), reference.len), *reference));
        } else {
            return Err(ParseError::TypeMismatch(format!("Tried to dereference {:?}", exp.1.var_type).to_string()));
        }
    }
    if lexer.find("&") {
        lexer.confirm();

        let (token, var) = parse_token_tree(string, structs, vars, functions)?;
        
        if let Token::Deref(token, _) = token {
            return Ok((*token, Var { var_type: VarType::Ref(Box::new(var.clone())), len: 1 }));
        } else {
            return Err(ParseError::InvalidOperator(format!("'*' on {:?}", var.var_type).to_string()));
        }
    } 

    if lexer.find("[") {
        if let Some(mut elements) = lexer.get_expansive("[", "]") {
            lexer.find("]");

            let mut lexer = Lexer::new(&mut elements);

            if let Some(element_type) = lexer.get_extensive_skip("[", "]", ";") {
                lexer.find(";");

                let (element, element_type) = parse_token_tree(element_type.clone(), 
                                                               structs,
                                                               vars,
                                                               functions)?;

                let len = match lexer.get_code().1.trim().parse::<usize>() {
                    Ok(i) => i,
                    Err(_) => return Err(ParseError::VariableParseFailiure(lexer.get_code().1.to_string())),
                };

                let var = Var {
                    len: len * element_type.len + 1,
                    var_type: VarType::Array(Box::new(element_type), len),
                };

                return Ok((Token::InitArray(vec![element; len]), var));
            }
        }
    }

    if let Some(array) = lexer.rget("[") {
        lexer.find("[");
        if let Some(index) = lexer.get("]") {
            lexer.find("]");
            lexer.confirm();

            let array = parse_token_tree(array, structs, vars, functions)?;
            let index = parse_token_tree(index, structs, vars, functions)?;

            if let VarType::Array(array_type, _) | VarType::Slice(array_type) = &array.1.var_type {
                if VarType::Int == index.1.var_type {
                    return Ok((
                        Token::Deref(Box::new(Token::Index(Box::new(array.0), 
                                                           Box::new(index.0), 
                                                           array_type.len)), 1), 
                        *array_type.clone()
                    ));
                } else {
                    return Err(ParseError::TypeMismatch(
                        format!("Tried to index array with '{:?}'", index.1.var_type).to_string()
                    ));
                }
            }
        }
    }

    let (var, var_type) = Variable::parse(string, vars)?;

    if let Variable::Ref(reference) = var {
        if let VarType::Ref(reference_type) = var_type.var_type {
            Ok((
                Token::Deref(
                    Box::new(
                        Token::Variable(
                            Variable::Ref(reference)
                        )
                    ),
                    reference_type.len
                ),
                *reference_type.clone()
            ))
        } else {
            Err(ParseError::MissingToken("Wut?"))
        }
    } else {
        Ok((
            Token::Variable(
                var
            ),
            var_type
        ))
    }
}

#[derive(Debug)]
pub struct Lexer<'l> {
    code: &'l mut String,
    cursor: usize,
    valid: Option<bool>,
}

impl<'l> Lexer<'l> {
    pub fn new(code: &'l mut String) -> Self {
        *code = code.trim().to_string();

        Lexer {
            code,
            cursor: 0,
            valid: None,
        }
    }

    pub fn restart(&mut self) {
        self.cursor = 0;
        self.valid = None;
    }

    pub fn confirm(&mut self) {
        *self.code = self.code.split_at(self.cursor).1.to_string();
        self.cursor = 0;
        self.valid = None;
    }

    pub fn get_code(&mut self) -> (&str, &str) {
        self.code.split_at(self.cursor)
    }

    pub fn is_valid(&mut self) -> bool {
        self.valid.unwrap_or(true)
    }

    pub fn validate(&mut self) {
        self.valid = Some(true);
    }

    pub fn skip(&mut self, num: usize) {
        self.cursor += num
    }

    pub fn skip_space(&mut self) -> bool {
        if !self.valid.unwrap_or(true) {
            return false;
        }

        let (_, code) = self.code.split_at(self.cursor);

        let next = code.find(|c: char| !c.is_whitespace());

        next.map(|next| {
            self.cursor += next;
        });

        self.valid.as_mut().map(|valid| *valid &= next.is_some());

        next.is_some()
    }

    pub fn find(&mut self, pattern: &'static str) -> bool {
        if !self.valid.unwrap_or(true) {
            return false;
        }

        let (_, code) = self.code.split_at(self.cursor);

        if code.starts_with(pattern) {
            self.cursor += pattern.len();

            true
        } else {
            self.valid.as_mut().map(|valid| *valid = false);

            false
        }
    }

    pub fn get(&mut self, pattern: &'static str) -> Option<String> {
        if !self.valid.unwrap_or(true) {
            return None;
        }

        let (_, code) = self.code.split_at(self.cursor);

        let stop = code.find(pattern);

        if let Some(stop) = stop {
            self.cursor += stop;

            return Some(code.split_at(stop).0.to_string());
        } else {
            self.valid.as_mut().map(|valid| *valid = false);

            return None;
        }
    }

    pub fn rget(&mut self, pattern: &'static str) -> Option<String> {
        if !self.valid.unwrap_or(true) {
            return None;
        }

        let (_, code) = self.code.split_at(self.cursor);

        let stop = code.rfind(pattern);

        if let Some(stop) = stop {
            self.cursor += stop;

            return Some(code.split_at(stop).0.to_string());
        } else {
            self.valid.as_mut().map(|valid| *valid = false);

            return None;
        }
    }



    pub fn get_any(&mut self, pattern: Vec<&'static str>) -> Option<String> {
        if !self.valid.unwrap_or(true) {
            return None;
        }

        let (_, code) = self.code.split_at(self.cursor);

        let stop = pattern.iter().map(|p| code.find(p)).filter(|s| s.is_some()).map(|s| s.unwrap()).min();

        if let Some(stop) = stop {
            self.cursor += stop;

            return Some(code.split_at(stop).0.to_string());
        } else {
            self.valid.as_mut().map(|valid| *valid = false);

            return None;
        }
    }

    pub fn get_expansive(&mut self, skip: &'static str, pattern: &'static str) -> Option<String> {
        if !self.valid.unwrap_or(true) {
            return None;
        }

        let code = self.code.split_at(self.cursor).1;

        let mut cursor = 0;

        let mut skip_count = 1;
        let mut pattern_count = 0;

        while skip_count > pattern_count {
            let code = code.split_at(cursor).1;

            let s = code.find(skip);
            let p = code.find(pattern);

            if s.is_some() || p.is_some() {
                let s = s.unwrap_or(code.len() + 1);
                let p = p.unwrap_or(code.len() + 1);

                if s < p {
                    skip_count += 1;
                } else if s == p {
                    panic!("The idiot writing the parser somehow set skip and pattern to the same fucking thing");
                } else {
                    pattern_count += 1;
                }

                cursor += s.min(p) + 1;
            } else {
                return None;
            }
        }

        self.cursor += cursor - 1;

        return Some(code.split_at(cursor - 1).0.to_string());
    }

    pub fn get_extensive_skip(&mut self, a: &'static str, b: &'static str, pattern: &'static str) -> Option<String> {
        if !self.valid.unwrap_or(true) {
            return None;
        }

        let code = self.code.split_at(self.cursor).1;

        let mut cursor = 0;

        let mut a_count = 0;
        let mut b_count = 0;
        
        loop {
            let code = code.split_at(cursor).1;

            if a_count <= b_count {
                let a = code.find(a).unwrap_or(code.len() + 1);
                let pattern = code.find(pattern).unwrap_or(code.len() + 1);

                if a < pattern {
                    a_count += 1;

                    cursor += a + 1;
                } else if a == pattern {
                    return None;
                } else {
                    cursor += pattern + 1;

                    break;
                }
            } else {
                let a = code.find(a).unwrap_or(code.len() + 1);
                let b = code.find(b).unwrap_or(code.len() + 1);

                if a < b {
                    a_count += 1;
                } else if a == b {
                    return None;
                } else {
                    b_count += 1;
                }

                cursor += a.min(b) + 1;
            }
        }

        self.cursor += cursor - 1;

        Some(code.split_at(cursor - 1).0.to_string())
    }
}


