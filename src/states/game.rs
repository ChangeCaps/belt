use fumarole::*;
use crate::isometric::*;
use std::sync::{Arc, Mutex};
use std::thread;
use std::collections::HashMap;

mod functions {
    use crate::language::*;
    use crate::isometric::*;

    pub fn goto(scope: &mut Scope, variables: Vec<Variable>) -> Variable {
        let x = match variables[0] {
            Variable::Int(x) => x,
            _ => return Variable::Null,
        };

        let y = match variables[1] {
            Variable::Int(y) => y,
            _ => return Variable::Null,
        };

        scope.script_data.lock().unwrap().instruction = super::Instruction::Goto(
            fumarole::Vec2::new(x as f32, y as f32)
        );

        while scope.script_data.lock().unwrap().instruction != super::Instruction::None {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        return Variable::Null;
    }

    pub fn grab(scope: &mut Scope, variables: Vec<Variable>) -> Variable { 
        let id = match variables[0] {
            Variable::Int(id) => id,
            _ => return Variable::Null,
        };

        scope.script_data.lock().unwrap().instruction = super::Instruction::Grab(id as usize);

        while scope.script_data.lock().unwrap().instruction != super::Instruction::None {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        return Variable::Null;
    }

    pub fn drop(scope: &mut Scope, _: Vec<Variable>) -> Variable { 
        scope.script_data.lock().unwrap().instruction = super::Instruction::Drop;

        while scope.script_data.lock().unwrap().instruction != super::Instruction::None {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        return Variable::Null;
    }

    pub fn crates(scope: &mut Scope, _: Vec<Variable>) -> Variable {
        let crates = &scope.script_data.lock().unwrap().crates;

        let mut new_location = 0;

        let mut heap = scope.heap.borrow_mut();
        let mut peekable = heap.iter().peekable();

        while let Some((location, _)) = peekable.next() {
            if let Some((next_location, _)) = peekable.peek() {
                if **next_location - location >= 6 * crates.len() {
                    new_location = location + 1;
                    break;
                }
            } else {
                new_location = *location + 1;
                break;
            }
        }

        for (i, (_, c)) in crates.iter().enumerate() {
            let index = i * 6 + new_location;

            let mut vector_fields = std::collections::HashMap::new();
            heap.insert(index + 2, Variable::Float(c.position.x));
            heap.insert(index + 3, Variable::Float(c.position.y));
            vector_fields.insert("x".to_string(), 1);
            vector_fields.insert("y".to_string(), 2);

            let mut fields = std::collections::HashMap::new();
            heap.insert(index + 1, Variable::Struct(vector_fields));
            heap.insert(index + 4, Variable::String(c.texture.to_string()));
            heap.insert(index + 5, Variable::Int(i as i32));
            fields.insert("position".to_string(), 1);
            fields.insert("type".to_string(), 4);
            fields.insert("id".to_string(), 5);

            heap.insert(index, Variable::Struct(fields));
        }

        return Variable::Slice(new_location, crates.len(), 6);
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
pub struct Belt {
    pub position: Vec2<i32>,
    pub texture: &'static str,
}

#[derive(Clone)]
pub struct Hole {
    pub position: Vec2<i32>,
    pub back_texture: &'static str,
    pub front_texture: &'static str,
}

#[derive(Clone)]
pub struct Crate {
    pub position: Vec2<f32>,
    pub texture: &'static str,
}

#[derive(Clone, Debug)]
pub struct Node {
    pub position: Vec2<i32>,
    pub occupant: Option<usize>,
    pub moving: bool,
    pub speed: f32,
}

#[derive(Clone)]
pub struct Game {
    pub claw: Claw,
    pub instruction: Instruction,
    pub next_crate: usize,
    pub belts: Vec<Belt>,
    pub holes: Vec<Hole>,
    pub crates: HashMap<usize, Crate>,
    pub falling_crates: Vec<Crate>,
    pub paths: Vec<Vec<Node>>,
    pub grabbed: Option<usize>,
    pub script_data: Arc<Mutex<ScriptData>>,
}

#[derive(Clone)]
pub struct ScriptData {
    pub claw: Claw,
    pub instruction: Instruction,
    pub crates: HashMap<usize, Crate>,
}

impl Game {
    pub fn new(code: String) -> Self {
        let script_data = ScriptData {
            claw: Claw {
                position: Vec2::new(0.0, 0.0),
                speed: 2.0
            },
            instruction: Instruction::None,
            crates: HashMap::new(),
        };

        let script_data_arc = Arc::new(Mutex::new(script_data.clone()));

        let mut game = Self {
            claw: script_data.claw,
            instruction: script_data.instruction,
            next_crate: 0,
            belts: Vec::new(),
            holes: Vec::new(),
            crates: HashMap::new(),
            falling_crates: Vec::new(),
            paths: vec![Vec::new()],
            grabbed: None,
            script_data: script_data_arc.clone(),
        };

        for i in 0..5 {
            let i = 4 - i;

            game.paths[0].push(Node {
                position: Vec2::new(0, i),
                occupant: None,
                moving: false,
                speed: 2.0/3.0,
            });

            game.belts.push(Belt {
                position: Vec2::new(0, i),
                texture: "belt",
            });
        }

        game.holes.push(Hole {
            position: Vec2::new(0, -1),
            back_texture: "hole_back_blue",
            front_texture: "hole_front_blue",
        });

        game.paths[0].push(Node {
            position: Vec2::new(0, -1),
            occupant: None,
            moving: false,
            speed: 2.0/3.0,
        });

        thread::spawn(move || {
            use crate::language::*;
            use crate::language;
            use functions::*;

            let mut interpreter = Interpreter::new();

            let mut vector_fields = std::collections::HashMap::new();
            vector_fields.insert("x".to_string(), Var { var_type: VarType::Float, len: 1 });
            vector_fields.insert("y".to_string(), Var { var_type: VarType::Float, len: 1 });

            interpreter.add_struct("vector", Struct {
                fields: vector_fields,
                len: 3,
            });

            let mut crate_fields = std::collections::HashMap::new();
            crate_fields.insert("position".to_string(), 
                                Var { var_type: VarType::Struct("vector".to_string()), len: 3 });
            crate_fields.insert("type".to_string(), Var { var_type: VarType::String, len: 1 });
            crate_fields.insert("id".to_string(), Var { var_type: VarType::Int, len: 1 });

            interpreter.add_struct("crate", Struct {
                fields: crate_fields,
                len: 6
            });

            interpreter.add_function("goto", 
                                     crate::function!(goto(VarType::Int, VarType::Int) -> VarType::Null));
            interpreter.add_function("grab", crate::function!(grab(VarType::Int) -> VarType::Null));
            interpreter.add_function("drop", crate::function!(drop() -> VarType::Null));
            interpreter.add_function("crates", crate::function!(crates() -> VarType::Slice(Box::new(Var  {
                var_type: VarType::Struct("crate".to_string()),
                len: 6,
            }))));

            interpreter.run(code, script_data_arc).unwrap();
        });

        game
    } 

    pub fn spawn_crate(&mut self, path: usize) -> Result<(), ()> {
        if self.paths[path][0].occupant.is_some() {
            return Err(());
        }

        self.paths[path][0].occupant = Some(self.crates.len());

        self.crates.insert(self.next_crate, Crate {
            position: Vec2::new(self.paths[path][0].position.x as f32,
                                self.paths[path][0].position.y as f32),
            texture: "crate_0"
        });

        self.next_crate += 1;

        Ok(())
    }

    pub fn move_arm(&mut self, target: Vec2<f32>, delta_time: f32) -> f32 {
        let distance = move_object(&mut self.claw.position, target, self.claw.speed, delta_time);
        
        if let Some(crate_id) = self.grabbed {
            move_object(&mut self.crates.get_mut(&crate_id).unwrap().position, 
                        target, 
                        self.claw.speed, 
                        delta_time);
        }

        distance
    }
}

pub fn move_object(object: &mut Vec2<f32>, target: Vec2<f32>, speed: f32, delta_time: f32) -> f32 {
    let difference = target - *object + Vec2::new(std::f32::EPSILON, 0.0);
    let distance = difference.magnitude();

    let step_length = (speed * delta_time).min(distance);

    *object += difference.normalize() * step_length;

    (target - *object + Vec2::new(std::f32::EPSILON, 0.0)).magnitude()
}

impl State for Game {
    fn draw(&self, frame: &mut Frame, data: &StateData) {
        // draw tiles
        for belt in &self.belts {
            frame.image(belt.texture)
                .position(belt.position.from_iso() - Vec2::new(0.5, 3.0))
                .depth(-belt.position.from_iso().y / 1000.0 - 0.1)
                .draw();
        }

        for hole in &self.holes {
            frame.image(hole.back_texture)
                .position(hole.position.from_iso() - Vec2::new(0.0, 6.0))
                .depth(-hole.position.from_iso().y / 1000.0 - 0.1)
                .draw();

            frame.image(hole.front_texture)
                .position(hole.position.from_iso() - Vec2::new(0.0, 6.0))
                .depth(-hole.position.from_iso().y / 1000.0 + 0.1)
                .draw();
        }

        // draw crates
        for (_, c) in &self.crates {
            frame.image(c.texture)
                .position(c.position.from_iso() + Vec2::new(0.0, 8.0))
                .depth(-c.position.from_iso().y / 1000.0)
                .draw();
        }


        // draw claw
        if self.grabbed.is_some() {
            frame.image("claw_back_closed")
                .position(self.claw.position.from_iso() - Vec2::new(0.0, 3.0))
                .pivot(Anchor::BottomMiddle)
                .depth(-self.claw.position.from_iso().y / 1000.0 - 0.01)
                .draw();
    
            frame.image("claw_closed")
                .position(self.claw.position.from_iso() - Vec2::new(0.0, 3.0))
                .pivot(Anchor::BottomMiddle)
                .depth(-self.claw.position.from_iso().y / 1000.0 + 0.01)
                .draw();
        } else {
            frame.image("claw_back_open")
                .position(self.claw.position.from_iso() - Vec2::new(0.0, 3.0))
                .pivot(Anchor::BottomMiddle)
                .depth(-self.claw.position.from_iso().y / 1000.0 - 0.01)
                .draw();
    
            frame.image("claw_open")
                .position(self.claw.position.from_iso() - Vec2::new(0.0, 3.0))
                .pivot(Anchor::BottomMiddle)
                .depth(-self.claw.position.from_iso().y / 1000.0 + 0.01)
                .draw();
        }
    }

    fn update(&mut self, data: &StateData) -> Transition {
        // synchronize script data
        let script_data = self.script_data.clone();
        let mut script_data = script_data.lock().unwrap();
        self.instruction = script_data.instruction.clone();
        
        let _ = self.spawn_crate(0);

        for path in &mut self.paths {
            let path_len = path.len();

            for i in 0..path_len - 1 {
                if path[i + 1].moving || path[i + 1].occupant.is_none() {
                    if let Some(crate_id) = path[i].occupant {
                        let distance = move_object(&mut self.crates.get_mut(&crate_id).unwrap().position,
                                                   Vec2::new(path[i + 1].position.x as f32,
                                                             path[i + 1].position.y as f32),
                                                   path[i].speed,
                                                   data.delta_time);

                        path[i].moving = true;

                        if distance < 0.05 && path[i + 1].occupant.is_none() {
                            if i == path_len - 2 {
                                path[i].moving = false;
                                path[i].occupant = None;

                                self.crates.remove(&crate_id);
                            } else {
                                path[i].moving = false;
                                path[i].occupant = None;
                                path[i + 1].occupant = Some(crate_id);
                            }
                        }
                    }
                }
            }
        }

        match self.instruction.clone() {
            Instruction::Goto(target) => {
                let distance = self.move_arm(target, data.delta_time);

                if distance < 0.05 {
                    self.instruction = Instruction::None;
                }
            },
            Instruction::Grab(crate_id) => {
                let distance = self.move_arm(self.crates[&crate_id].position, data.delta_time);

                if distance < 0.05 {
                    for path in &mut self.paths {
                        for node in path {
                            if node.occupant == Some(crate_id) {
                                node.occupant = None;
                                node.moving = false;
                            }
                        }
                    }

                    self.grabbed = Some(crate_id);
                    self.instruction = Instruction::None;
                }
            },
            Instruction::Drop => {
                self.grabbed = None;
                self.instruction = Instruction::None;
            },
            Instruction::None => ()
        }

        script_data.instruction = self.instruction.clone();
        script_data.claw = self.claw.clone();
        script_data.crates = self.crates.clone();

        Transition::None
    }
}
