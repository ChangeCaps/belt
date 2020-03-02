use fumarole::*;
use crate::isometric::*;
use crate::random::*;
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

        return Variable::Null
    }

    pub fn grab(scope: &mut Scope, variables: Vec<Variable>) -> Variable { 
        if scope.script_data.lock().unwrap().claw.grabbed.is_some() {
            scope.script_data.lock().unwrap().instruction = super::Instruction::None;

            return Variable::Bool(false);
        }

        let id = match variables[0] {
            Variable::Int(id) => id,
            _ => return Variable::Bool(false),
        };

        scope.script_data.lock().unwrap().instruction = super::Instruction::Grab(id as usize);

        while scope.script_data.lock().unwrap().instruction != super::Instruction::None {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        return Variable::Bool(true);
    }

    pub fn drop(scope: &mut Scope, variables: Vec<Variable>) -> Variable { 
         let x = match variables[0] {
            Variable::Int(x) => x,
            _ => return Variable::Bool(false),
        };

        let y = match variables[1] {
            Variable::Int(y) => y,
            _ => return Variable::Bool(false),
        };

        scope.script_data.lock().unwrap().instruction = super::Instruction::Drop(super::Vec2::new(x, y));

        while scope.script_data.lock().unwrap().instruction != super::Instruction::None {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        if scope.script_data.lock().unwrap().instruction == super::Instruction::Failiure {
            scope.script_data.lock().unwrap().instruction = super::Instruction::None;

            return Variable::Bool(false);
        }

        return Variable::Bool(true);
    }

    pub fn crates(scope: &mut Scope, _: Vec<Variable>) -> Variable {
        let crates = &scope.script_data.lock().unwrap().crates;

        let mut new_location = 0;

        let mut heap = scope.heap.borrow_mut();
        let mut peekable = heap.iter().collect::<Vec<_>>();
        peekable.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());
        let mut peekable = peekable.iter().peekable();

        while let Some((location, _)) = peekable.next() {
            if let Some((next_location, _)) = peekable.peek() {
                if **next_location - **location >= 6 * crates.len() {
                    new_location = **location + 1;
                    break;
                }
            } else {
                new_location = **location + 1;
                break;
            }
        }

        for (i, (id, c)) in crates.iter().enumerate() {
            let index = i * 6 + new_location;

            let mut vector_fields = std::collections::HashMap::new();
            heap.insert(index + 2, Variable::Float(c.position.x));
            heap.insert(index + 3, Variable::Float(c.position.y));
            vector_fields.insert("x".to_string(), 1);
            vector_fields.insert("y".to_string(), 2);

            let mut fields = std::collections::HashMap::new();
            heap.insert(index + 1, Variable::Struct(vector_fields));
            heap.insert(index + 4, Variable::String(c.texture.to_string()));
            heap.insert(index + 5, Variable::Int(*id as i32));
            fields.insert("position".to_string(), 1);
            fields.insert("type".to_string(), 4);
            fields.insert("id".to_string(), 5);

            heap.insert(index, Variable::Struct(fields));
        }

        return Variable::Slice(new_location, crates.len(), 6);
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Instruction {
    None,
    Failiure,
    Goto(Vec2<f32>),
    Grab(usize),
    Drop(Vec2<i32>),
}

#[derive(Clone)]
pub struct Claw {
    pub position: Vec2<f32>,
    pub height: f32,
    pub travel_height: f32,
    pub grabbed: Option<usize>,
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

#[derive(Clone, Debug)]
pub struct Crate {
    pub position: Vec2<f32>,
    pub height: f32,
    pub texture: &'static str,
}

#[derive(Clone)]
pub struct FallingCrate {
    pub position: Vec2<f32>,
    pub speed: f32,
    pub depth: f32,
    pub texture: &'static str,
}

#[derive(Clone)]
pub struct Tile {
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
    pub tiles: HashMap<(i32, i32), Tile>,
    pub crates: HashMap<usize, Crate>,
    pub falling_crates: Vec<FallingCrate>,
    pub paths: Vec<Vec<Node>>,
    pub script_data: Arc<Mutex<ScriptData>>,
    pub random: Random,
    pub level_crates: Vec<(i32, &'static str)>,
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
                height: 20.0,
                travel_height: 20.0,
                grabbed: None,
                speed: 3.0,
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
            tiles: HashMap::new(),
            crates: HashMap::new(),
            falling_crates: Vec::new(),
            paths: vec![Vec::new(), Vec::new()],
            script_data: script_data_arc.clone(),
            random: Random::new(13124),
            level_crates: vec![
                (30, "crate_1"),
                (70, "crate_0")
            ]
        };

        for x in -10..10 {
            for y in -10..10 {
                game.tiles.insert((x, y), Tile {
                    texture: "floor",
                });
            }
        }

        for i in 0..9 {
            let i = 8 - i;

            game.paths[0].push(Node {
                position: Vec2::new(0, i),
                occupant: None,
                moving: false,
                speed: 2.0/3.0,
            });

            game.spawn_belt(Belt {
                position: Vec2::new(0, i),
                texture: "belt",
            });
        }

        for i in 0..4 {
            let i = 3 - i;

            game.paths[1].push(Node {
                position: Vec2::new(1, i),
                occupant: None,
                moving: false,
                speed: 2.0/3.0,
            });

            game.spawn_belt(Belt {
                position: Vec2::new(1, i),
                texture: "belt",
            });
        }

        game.spawn_hole(Hole {
            position: Vec2::new(0, -1),
            back_texture: "hole_back_blue",
            front_texture: "hole_front_blue",
        });

        game.spawn_hole(Hole {
            position: Vec2::new(1, -1),
            back_texture: "hole_back_blue",
            front_texture: "hole_front_blue",
        });

        game.paths[0].push(Node {
            position: Vec2::new(0, -1),
            occupant: None,
            moving: false,
            speed: 2.0/3.0,
        });

        game.paths[1].push(Node {
            position: Vec2::new(1, -1),
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
            interpreter.add_function("grab", crate::function!(grab(VarType::Int) -> VarType::Bool));
            interpreter.add_function("drop", crate::function!(drop(VarType::Int, VarType::Int) -> VarType::Bool));
            interpreter.add_function("crates", crate::function!(crates() -> VarType::Slice(Box::new(Var  {
                var_type: VarType::Struct("crate".to_string()),
                len: 6,
            }))));

            interpreter.run(code, script_data_arc).expect("interpreter fail");
        });

        game
    } 

    pub fn spawn_belt(&mut self, belt: Belt) {
        self.tiles.remove(&(belt.position.x, belt.position.y));
        self.belts.push(belt);
    }

    pub fn spawn_hole(&mut self, hole: Hole) {
        self.tiles.remove(&(hole.position.x, hole.position.y));
        self.holes.push(hole);
    }

    pub fn spawn_crate(&mut self, path: usize) -> Result<(), ()> {
        if self.paths[path][0].occupant.is_some() {
            return Err(());
        }

        self.paths[path][0].occupant = Some(self.next_crate);

        let crate_type = loop {
            let crate_index = self.random.range_i32(0, self.level_crates.len() as i32 - 1) as usize;

            if self.random.range_i32(1, 100) < self.level_crates[crate_index].0 {
                break self.level_crates[crate_index].1;
            }
        };

        self.crates.insert(self.next_crate, Crate {
            position: Vec2::new(self.paths[path][0].position.x as f32,
                                self.paths[path][0].position.y as f32),
            texture: crate_type,
            height: 0.0,
        });

        self.next_crate += 1;

        Ok(())
    }

    pub fn move_arm(&mut self, target: Vec2<f32>, target_height: f32, delta_time: f32) -> bool {
        let distance = move_object(&mut self.claw.position, target, self.claw.speed, delta_time);

        let height_difference = target_height - self.claw.height;

        if distance < 0.01 {
            let height_step = (height_difference) / (height_difference.abs() + std::f32::EPSILON);

            self.claw.height += height_step * ((self.claw.speed * 12.0) * delta_time).min(height_difference.abs());
        }
        
        if let Some(crate_id) = self.claw.grabbed {
            move_object(&mut self.crates.get_mut(&crate_id).expect("crate doesnt exist").position, 
                        target, 
                        self.claw.speed, 
                        delta_time);
                        
            self.crates.get_mut(&crate_id).expect("crate doesnt exist").height = self.claw.height;
        }

        distance < 0.01 && height_difference.abs() < 0.01
    }
}

pub fn move_object(object: &mut Vec2<f32>, target: Vec2<f32>, speed: f32, delta_time: f32) -> f32 {
    let difference = target - *object + Vec2::new(std::f32::EPSILON, 0.0);
    let distance = difference.magnitude();

    let step_length = (speed * delta_time).min(distance) + std::f32::EPSILON;

    *object += difference.normalize() * step_length;

    (target - *object + Vec2::new(std::f32::EPSILON, 0.0)).magnitude()
}

impl State for Game {
    fn draw(&self, frame: &mut Frame, data: &StateData) {
        // draw tiles
        for belt in &self.belts {
            frame.image(belt.texture)
                .position(belt.position.from_iso() - Vec2::new(2.5, 3.0))
                .depth(-belt.position.from_iso().y / 1000.0 - 0.011)
                .pixel_scale(1.0)
                .draw();
        }

        for hole in &self.holes {
            frame.image(hole.back_texture)
                .position(hole.position.from_iso() - Vec2::new(0.0, 7.0))
                .depth(-hole.position.from_iso().y / 1000.0 - 0.2)
                .pixel_scale(1.0)
                .draw();

            frame.image(hole.front_texture)
                .position(hole.position.from_iso() - Vec2::new(0.0, 7.0))
                .depth(-hole.position.from_iso().y / 1000.0 + std::f32::EPSILON)
                .pixel_scale(1.0)
                .draw();
        }

        // draw crates
        for (_, c) in &self.crates {
            frame.image(c.texture)
                .position(c.position.from_iso() + Vec2::new(0.0, 8.0 + c.height))
                .depth(-(c.position.from_iso().y - c.height) / 1000.0 + std::f32::EPSILON)
                .pixel_scale(1.0)
                .draw();
        }

        // draw falling crates
        for c in &self.falling_crates {
            frame.image(c.texture)
                .position(c.position + Vec2::new(0.0, 8.0))
                .depth(c.depth + std::f32::EPSILON)
                .pixel_scale(1.0)
                .draw();
        }

        // draw tiles
        for (position, tile) in &self.tiles {
            let position = Vec2::new(position.0, position.1).from_iso();

            frame.image(tile.texture)
                .position(position - Vec2::new(0.0, 7.0))
                .depth(-position.y / 1000.0 - 0.01)
                .pixel_scale(1.0)
                .draw();
        }

        let claw_position = self.claw.position.from_iso() - Vec2::new(0.0, 3.0 - self.claw.height);

        // draw claw 
        if self.claw.grabbed.is_some() {           
            frame.image("claw_back_closed")
                .position(claw_position)
                .pivot(Anchor::BottomMiddle)
                .depth(-(self.claw.position.from_iso() - self.claw.height).y / 1000.0 - 0.01)
                .pixel_scale(1.0)
                .draw();
    
            frame.image("claw_closed")
                .position(claw_position)
                .pivot(Anchor::BottomMiddle)
                .depth(-(self.claw.position.from_iso() - self.claw.height).y / 1000.0 + 0.015)
                .pixel_scale(1.0)
                .draw();
        } else {
            frame.image("claw_back_open")
                .position(claw_position)
                .pivot(Anchor::BottomMiddle)
                .depth(-(self.claw.position.from_iso() - self.claw.height).y / 1000.0 - 0.01)
                .pixel_scale(1.0)
                .draw();
    
            frame.image("claw_open")
                .position(claw_position)
                .pivot(Anchor::BottomMiddle)
                .depth(-(self.claw.position.from_iso() - self.claw.height).y / 1000.0 + 0.015)
                .pixel_scale(1.0)
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
                        let distance = move_object(&mut self.crates.get_mut(&crate_id).expect("1").position,
                                                   Vec2::new(path[i + 1].position.x as f32,
                                                             path[i + 1].position.y as f32),
                                                   path[i].speed,
                                                   data.delta_time);

                        path[i].moving = true;

                        if distance < 0.05 && path[i + 1].occupant.is_none() {
                            if i == path_len - 2 {
                                path[i].moving = false;
                                path[i].occupant = None;

                                let removed_crate = self.crates.remove(&crate_id).expect("2");
                                self.falling_crates.push(FallingCrate {
                                    position: removed_crate.position.from_iso(),
                                    speed: 0.0,
                                    depth: -removed_crate.position.from_iso().y / 1000.0,
                                    texture: removed_crate.texture,
                                });
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

        let mut i = 0;
        while i < self.falling_crates.len() {
            self.falling_crates[i].speed += 96.0 * data.delta_time;
            self.falling_crates[i].position.y -= self.falling_crates[i].speed * data.delta_time;

            if self.falling_crates[i].position.y < -240.0 {
                self.falling_crates.remove(i);

                continue;
            }

            i += 1;
        }

        match self.instruction.clone() {
            Instruction::Goto(target) => {
                let target_reached = self.move_arm(target, self.claw.travel_height, data.delta_time);

                if target_reached {
                    self.instruction = Instruction::None;
                }
            },
            Instruction::Grab(crate_id) => {
                if let Some(c) = self.crates.get(&crate_id) {
                    let c = c.clone();
                    
                    let target_reached = self.move_arm(c.position, 
                                                       c.height,
                                                       data.delta_time);

                    if target_reached {
                        'outer: for path in &mut self.paths {
                            for node in path {
                                if node.occupant == Some(crate_id) {
                                    node.occupant = None;
                                    node.moving = false;
                                    break 'outer;
                                }
                            }
                        }

                        self.claw.grabbed = Some(crate_id);
                        self.instruction = Instruction::Goto(c.position + std::f32::EPSILON);
                    }
                } else {
                    self.instruction = Instruction::Failiure;
                }
            },
            Instruction::Drop(target) => { 
                let mut target_height = -7.0;

                let mut target_node = None;

                'outer: for (path_index, path) in self.paths.iter().enumerate() {
                    for (node_index, node) in path.iter().enumerate() {
                        if node.position == target {
                            target_node = Some((path_index, node_index));
                            target_height = 0.0;
                            break 'outer;
                        }
                    }
                }

                let target_reached = self.move_arm(Vec2::new(target.x as f32, target.y as f32), 
                                                   target_height,
                                                   data.delta_time);

                if target_reached {
                    if let Some(grabbed) = self.claw.grabbed {
                        self.crates.get_mut(&grabbed).expect("6").height = target_height; 

                        if let Some((path, node)) = target_node {
                            if self.paths[path][node].occupant.is_none() {
                                self.paths[path][node].occupant = self.claw.grabbed;
                                self.claw.grabbed = None;
                            }
                        } else {
                            self.claw.grabbed = None;
                        }

                        self.instruction = Instruction::Goto(Vec2::new(target.x as f32, target.y as f32));
                    } else {
                        self.instruction = Instruction::Failiure;
                    }
                }
            },
            Instruction::None => {
                self.move_arm(self.claw.position, self.claw.travel_height, data.delta_time);
            },
            Instruction::Failiure => {
                self.instruction = Instruction::None;
            },
        }

        script_data.instruction = self.instruction.clone();
        script_data.claw = self.claw.clone();
        script_data.crates = self.crates.clone();

        Transition::None
    }
}
