extern crate fumarole;

pub mod states;
pub mod isometric;
pub mod language;

fn print(variables: Vec<language::Variable>) -> language::Variable {
    println!("{:?}", variables[0]);

    language::Variable::Null
}

fn main() {
    use language::*;

    let mut interpreter = Interpreter::new();

    interpreter.add_function("print", function!(print{Any}));
    
    interpreter.run(r#"

struct s1 {
    test1: int
}

struct s2 {
    test2: s1
}

fn test() -> s1 {
    let x = s1 { test1: 2 };

    return x;
}

let y = test();

print(y);
    "#).unwrap();

    /*fumarole::Application::new()
        .run(|loader| {
            Box::new(states::Game{

            })
        });*/

}
