use fumarole::*;
use crate::*;
use level::*;
use isometric::*;

pub enum MenuItem {
    
}

pub struct Menu {
    pub selected_menu_item: usize,
    pub menu_items: Vec<MenuItem>,
}

#[derive(Clone)]
pub struct LevelEditor {
    level: Level,
    selected_node: Option<usize>,
    path: String,
}

impl LevelEditor {
    pub fn new(path: String) -> LevelEditor {
        LevelEditor {
            level: Level::new(),
            selected_node: None,
            path,
        }
    }

    pub fn load(path: String) -> LevelEditor {
        LevelEditor {
            level: Level::load(path.clone()),
            selected_node: None,
            path,
        }
    }
}

impl State for LevelEditor {
    fn draw(&self, frame: &mut Frame, _data: &StateData) {
        self.level.draw(frame);

        for spawner in &self.level.spawners {
            for (_, node) in &spawner.nodes {
                {
                    let mut node = *node;

                    while let Some(next) = self.level.nodes[node].next {
                        let p0 = self.level.nodes[node].position.from_iso() + Vec2::new(0.0, 7.0);
                        let p1 = self.level.nodes[next].position.from_iso() + Vec2::new(0.0, 7.0);

                        frame.line()
                            .points(p0, p1)
                            .smooth(true)
                            .color(color::rgb(0.7, 0.6, 0.1))
                            .width(10.0)
                            .depth(1.0)
                            .draw();
                        
                        node = next;
                    }
                }

                let node = &self.level.nodes[*node];

                frame.ellipse()
                    .position(node.position.from_iso() + Vec2::new(0.0, self.level.get_height(node.position)))
                    .size(Vec2::new(24.0, 12.0))
                    .color(color::rgb(0.7, 0.6, 0.1))
                    .depth(1.0)
                    .draw();
            }
        }
    }

    fn update(&mut self, data: &StateData) -> Transition {
        if data.key_pressed(KeyCode::Return) {
            self.level.save(self.path.clone());
        }

        if data.key_pressed(KeyCode::R) {
            let position = data.mouse_position.into_iso(); 
            let tile = self.level.get_tile_mut(position).unwrap();

            if let Tile::Hole(_, _) | Tile::Belt(_) = tile {
                *tile = Tile::Floor("floor".to_string());

                for (i, node) in self.level.nodes.clone().iter().enumerate() {
                    if node.position == position {
                        self.level.remove_path(i);
                    }
                }
            }
        }

        if data.mouse_pressed(MouseButton::Left) {
            let position = data.mouse_position.into_iso(); 
            let tile = self.level.get_tile_mut(position).unwrap();

            match tile.clone() {
                Tile::Floor(_) => {
                    *tile = Tile::Hole("hole_back_blue".to_string(), "hole_front_blue".to_string());
     
                    let index = self.level.nodes.len();
     
                    self.level.nodes.push(
                        Node {
                            position,
                            occupant: None,
                            next: None,
                            moving: false,
                            speed: 2.0/3.0,
                        }
                    );
     
                    self.level.spawners.push(
                        Spawner {
                            time: 0.0,
                            max_time: 2.0,
                            crates: vec![(100, "crate_1".to_string())],
                            nodes: vec![(100, index)],
                        }
                    );
     
                    self.selected_node = Some(index);
                },
                Tile::Belt(_) | Tile::Hole(_, _) => {
                    *tile = Tile::Hole("hole_back_blue".to_string(), "hole_front_blue".to_string());

                    for (i, node) in self.level.nodes.clone().iter().enumerate() {
                        if node.position == position {
                            node.next.map(|node| { 
                                self.level.remove_path(node);
                            });

                            self.selected_node = Some(i);
                        }
                    }
                },
            }
        }

        if data.mouse_held(MouseButton::Left) {
            if let Some(selected_node) = self.selected_node {
                let position = data.mouse_position.into_iso();
                let selected_position = self.level.nodes[selected_node].position;
    
                let difference = position - selected_position;
                let difference = Vec2::new(difference.x as f32, difference.y as f32);

                if difference.magnitude() < 1.1 {
                    if let Tile::Floor(_) = self.level.get_tile(position).unwrap() { 
                        *self.level.get_tile_mut(position).unwrap() = 
                            Tile::Hole("hole_back_blue".to_string(), "hole_front_blue".to_string());
    
                        let index = self.level.nodes.len();
    
                        self.level.nodes.push(
                            Node {
                                position,
                                occupant: None,
                                next: None,
                                moving: false,
                                speed: 2.0/3.0,
                            }
                        ); 
    
                        self.level.nodes[selected_node].next = Some(index);
    
                        // update previous tiles
                        let mut node = selected_node;
    
                        while let Some(next) = self.level.nodes[node].next {
                            let position = self.level.nodes[node].position;
                            let tile = self.level.get_tile_mut(position).unwrap();
    
                            *tile = Tile::Belt("belt".to_string());
    
                            node = next;
                        }
    
                        // update selected node
                        self.selected_node = Some(index);
                    }
                }
            }
        }

        if data.mouse_released(MouseButton::Left) {
            self.selected_node = None;
        }
        
        Transition::None
    }
}
