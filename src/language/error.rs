#[derive(Clone, Debug)]
pub enum Error {
    ParseError(ParseError),
    InterpreterError(InterpreterError),
}

#[derive(Clone, Debug)]
pub enum InterpreterError {
    NullReference,
    DereferenceingNonReference,
    InvalidOperator {
        operator: &'static str,
        lhs: &'static str,
        rhs: &'static str,
    },
    InvalidFunctionCall(&'static str),
    InvalidIf(&'static str),
    IndexError(&'static str),
    InvalidOperatorApplication(String),
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnrecognizedToken,
    StringNotEnded,
    MissingToken(&'static str),
    UnexpectedToken(String),
    EmptyTokenTree,
    InvalidParameterIdent(String),
    InvalidFunctionIdent(String),
    VariableParseFailiure(String),
    InvalidLeftHand(String),
    UnsetVariable(String),
    InvalidOperator(String),
    UnrecognizedField(String, String),
    TypeMismatch(String),
    TooFewParameters,
    TooManyParameters,
    UnrecognizedFunction(String),
}