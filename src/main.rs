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
fn test(t: &[int; 2]) {
    let x = *t;

    print(x[0]);
}

let x = [1; 2];

x[0] = 2;

test(&x);
    "#).unwrap();

    /*fumarole::Application::new()
        .run(|loader| {
            Box::new(states::Game{

            })
        });*/

}
