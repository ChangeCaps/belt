extern crate fumarole;

pub mod states;
pub mod isometric;
pub mod language;

fn print(_: &mut language::Scope, variables: Vec<language::Variable>) -> language::Variable {
    println!("{:?}", variables[0]);

    language::Variable::Null
}

fn main() {
    use language::*;

    let mut interpreter = Interpreter::new();

    interpreter.add_function("print", function!(print(language::VarType::Any) -> Null));
    
    interpreter.run(r#"
struct test {
    test: int,
    test2: int,
}

let t = test {
    test: 0,
    test2: 3
};

let x = [t; 5];

let y = x;

print(x);
    "#).unwrap();

    /*fumarole::Application::new()
        .run(|loader| {
            Box::new(states::Game{

            })
        });*/
}
