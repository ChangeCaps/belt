use fumarole::*;
use std::sync::{Arc, Mutex};
use std::thread;

mod functions {
    use crate::language::*;

    pub fn goto(scope: &mut Scope, variables: Vec<Variable>) -> Variable {
        while scope.script_data.lock().unwrap().instruction != super::Instruction::None {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        let x = match variables[0] {
            Variable::Float(x) => x * 48.0,
            _ => return Variable::Null,
        };

        let y = match variables[1] {
            Variable::Float(y) => y * 48.0,
            _ => return Variable::Null,
        };

        scope.script_data.lock().unwrap().instruction = super::Instruction::Goto(
            fumarole::Vec2::new(x, y)
        );

        return Variable::Null;
    }
}

#[derive(Clone, PartialEq)]
pub enum Instruction {
    None,
    Goto(Vec2<f32>),
    Grab(usize),
    Drop,
}

#[derive(Clone)]
pub struct Claw {
    pub position: Vec2<f32>,
    pub speed: f32,
}

#[derive(Clone)]
pub struct Tile {
    pub position: Vec2<f32>,
    pub texture: &'static str,
}

#[derive(Clone)]
pub struct Crate {
    pub position: Vec2<f32>,
    pub texture: &'static str,
}

#[derive(Clone)]
pub struct Game {
    pub claw: Claw,
    pub instruction: Instruction,
    pub tiles: Vec<Tile>,
    pub script_data: Arc<Mutex<ScriptData>>,
}

#[derive(Clone)]
pub struct ScriptData {
    pub claw: Claw,
    pub instruction: Instruction,
}

impl Game {
    pub fn new(code: String) -> Self {
        let script_data = ScriptData {
            claw: Claw {
                position: Vec2::new(0.0, 0.0),
                speed: 48.0
            },
            instruction: Instruction::None,
        };

        let script_data_arc = Arc::new(Mutex::new(script_data.clone()));

        let mut game = Self {
            claw: script_data.claw,
            instruction: script_data.instruction,
            tiles: Vec::new(),
            script_data: script_data_arc.clone(),
        };

        for i in 0..10 {
            game.tiles.push(Tile {
                position: Vec2::new(i as f32 * 24.0, i as f32 * 12.0),
                texture: "belt",
            });
        }

        thread::spawn(move || {
            use crate::language::*;
            use crate::language;
            use functions::*;

            let mut interpreter = Interpreter::new();

            interpreter.add_function("goto", crate::function!(goto(VarType::Float, VarType::Float) -> Float));

            interpreter.run(code, script_data_arc).unwrap();
        });

        game
    }

    pub fn move_arm(&mut self, target: Vec2<f32>, delta_time: f32) {
        let difference = target / Vec2::new(1.0, 2.0) - self.claw.position + Vec2::new(std::f32::EPSILON, 0.0);
        let distance = difference.magnitude();

        let step_length = (self.claw.speed * delta_time)
                                  .min(distance);

        self.claw.position += difference.normalize() * step_length * (difference.normalize().x.abs() / 2.0 + 0.5);

        if distance < 0.5 {
            self.instruction = Instruction::None;
        }
    }
}

impl State for Game {
    fn draw(&self, frame: &mut Frame, data: &StateData) {
        // draw tiles
        for tile in &self.tiles {
            frame.image(tile.texture)
                .position(tile.position)
                .depth(-tile.position.y / 1000.0)
                .draw();
        }


        // draw claw
        frame.image("claw_back_open")
            .position(self.claw.position - Vec2::new(0.0, 13.0))
            .pivot(Anchor::BottomMiddle)
            .draw();

        frame.image("claw_open")
            .position(self.claw.position - Vec2::new(0.0, 13.0))
            .pivot(Anchor::BottomMiddle)
            .draw();
    }

    fn update(&mut self, data: &StateData) -> Transition {
        // synchronize script data
        let script_data = self.script_data.clone();
        let mut script_data = script_data.lock().unwrap();
        self.instruction = script_data.instruction.clone();

        match self.instruction.clone() {
            Instruction::Goto(target) => {
                self.move_arm(target, data.delta_time);
            },
            Instruction::Grab(crate_id) => {
                //self.move_arm()
            },
            Instruction::Drop => {

            },
            Instruction::None => ()
        }

        script_data.instruction = self.instruction.clone();
        script_data.claw = self.claw.clone();

        Transition::None
    }
}