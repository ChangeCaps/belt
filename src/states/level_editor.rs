use fumarole::*;
use crate::*;
use level::*;
use isometric::*;

const CRATES: &'static [&'static str] = &[
    "crate_0",
    "crate_1"
];

const CRATE_MENU: Menu<(i32, String)> = Menu {
    items: &[
        MenuItem {
            draw: &|c| format!("type: {}", c.1),
            select: &|_| {},
            right: &|(_, c)| {
                // FIXME: the index of the texture should be stored alongside the texture but i just cant be bothered
                // therefore we just retreive the index by using find on an iterator over the textures
                let (index, _) = CRATES.iter().enumerate().find(|(_, _c)| **_c == *c).unwrap();

                // swap to the next texture and wrap around
                *c = CRATES[(index + 1) % CRATES.len()].to_string();
            },
            left: &|(_, c)| {
                // FIXME: the index of the texture should be stored alongside the texture but i just cant be bothered
                // therefore we just retreive the index by using find on an iterator over the textures
                let (index, _) = CRATES.iter().enumerate().find(|(_, _c)| **_c == *c).unwrap();

                // since the modulus operator doenst work when the left hand number goes below zero, we add the right hand
                // number minus one, to achieve the desired effect
                *c = CRATES[(index + CRATES.len() - 1) % CRATES.len()].to_string();
            },
        },
        MenuItem {
            draw: &|c| format!("chance: {}", c.0),
            select: &|_| {},
            right: &|(chance, _)| {
                *chance += 5;
            },
            left: &|(chance, _)| {
                *chance -= 5;
            },
        },
    ],
    selected_item: 0,
};

const SPAWNER_MENU: Menu<Spawner> = Menu {
    items: &[
        MenuItem {
            draw: &|spawner| format!("red  : {:.3}", spawner.color[0]),
            select: &|_| {},
            right: &|spawner| spawner.color[0] += 0.01,
            left: &|spawner| spawner.color[0] -= 0.01,
        },
        MenuItem {
            draw: &|spawner| format!("green: {:.3}", spawner.color[1]),
            select: &|_| {},
            right: &|spawner| spawner.color[1] += 0.01,
            left: &|spawner| spawner.color[1] -= 0.01,
        },
        MenuItem {
            draw: &|spawner| format!("blue : {:.3}", spawner.color[2]),
            select: &|_| {},
            right: &|spawner| spawner.color[2] += 0.01,
            left: &|spawner| spawner.color[2] -= 0.01,
        },
        MenuItem {
            draw: &|_| "add crate".to_string(),
            select: &|spawner| spawner.crates.push((100, CRATES[0].to_string())),
            right: &|_| {},
            left: &|_| {},
        }
    ],
    selected_item: 0,
};

#[derive(Clone)]
pub struct MenuItem<T: 'static> {
    pub draw: &'static (dyn Fn(&T) -> String + Send + Sync), 
    pub select: &'static (dyn Fn(&mut T) -> () + Send + Sync),
    pub right: &'static (dyn Fn(&mut T) -> () + Send + Sync),
    pub left: &'static (dyn Fn(&mut T) -> () + Send + Sync),
}

#[derive(Clone)]
pub struct Menu<T: 'static> {
    pub items: &'static [MenuItem<T>],
    pub selected_item: usize,
}

impl<T> Menu<T> {
    pub fn draw(&self, frame: &mut Frame, transform: Transform, t: &T) {
        for (i, item) in self.items.iter().enumerate() {
            let draw = &item.draw;

            let start = if i == self.selected_item {
                ">"
            } else {
                " "
            };

            frame.text("assets/font.ttf")
                .text(format!("{} {}", start, draw(t)))
                .parent(transform)
                .position(Vec2::new(0.0, i as f32 * -6.0))
                .pivot(Anchor::TopLeft)
                .scale(10.0)
                .depth(2.0)
                .draw();
        }
    }

    pub fn update(&mut self, data: &StateData, t: &mut T) {
        if data.key_pressed(KeyCode::Up) {
            self.selected_item = (self.selected_item + self.items.len() - 1) % self.items.len();
        }
        
        if data.key_pressed(KeyCode::Down) {
            self.selected_item = (self.selected_item + self.items.len() + 1) % self.items.len();
        }

        if data.key_pressed(KeyCode::Right) {
            let right = &self.items[self.selected_item].right;

            right(t);
        }

        if data.key_pressed(KeyCode::Left) {
            let left = &self.items[self.selected_item].left;

            left(t);
        }

        if data.key_pressed(KeyCode::Return) {
            let select = &self.items[self.selected_item].select;

            select(t);
        }
    }
}

#[derive(Clone)]
pub struct LevelEditor {
    level: Level,
    selected_node: Option<usize>,
    selected_spawner: Option<usize>,
    selected_crate: Option<usize>,
    show_spawners_menu: bool,
    spawner_menu: Menu<Spawner>,
    crate_menu: Menu<(i32, String)>,
    path: String,
    random: random::Random,
}

impl LevelEditor {
    pub fn new(path: String) -> LevelEditor {
        LevelEditor {
            level: Level::new(),
            selected_node: None,
            selected_spawner: None,  
            selected_crate: None,
            show_spawners_menu: false,
            spawner_menu: SPAWNER_MENU, 
            crate_menu: CRATE_MENU,
            path,
            random: random::Random::new(655835),
        }
    }

    pub fn load(path: String) -> LevelEditor {
        LevelEditor {
            level: Level::load(path.clone()),
            selected_node: None,
            selected_spawner: None,
            selected_crate: None,
            show_spawners_menu: false,
            spawner_menu: SPAWNER_MENU,
            crate_menu: CRATE_MENU,
            path,
            random: random::Random::new(655835),
        }
    }

    pub fn draw_spawner_menu(&self, frame: &mut Frame) {
        if !self.show_spawners_menu {
            return;
        }

        frame.rect()
            .size(Vec2::new(100.0, 150.0))
            .anchor(Anchor::TopLeft)
            .pivot(Anchor::TopLeft)
            .color(color::rgb(0.7, 0.2, 0.1))
            .depth(1.0)
            .draw();

        frame.text("assets/font.ttf")
            .text("Spawners")
            .position(Vec2::new(5.0, 0.0))
            .scale(20.0)
            .pivot(Anchor::TopLeft)
            .anchor(Anchor::TopLeft)
            .depth(1.1)
            .draw();

        for (i, spawner) in self.level.spawners.iter().enumerate() {
            frame.rect()
                .size(Vec2::new(60.0, 10.0))
                .position(Vec2::new(12.5, i as f32 * -12.5 - 25.0))
                .color(spawner.color)
                .anchor(Anchor::TopLeft)
                .pivot(Anchor::TopLeft)
                .depth(1.1)
                .draw();
        }

        self.selected_spawner.map(|spawner| {
            let mut transform = Transform::new();
            transform.position = Vec2::new(-77.5, 100.0);

            self.spawner_menu.draw(frame, transform, &self.level.spawners[spawner]);

            frame.rect()
                .size(Vec2::new(70.0, 150.0))
                .position(Vec2::new(150.0, 0.0))
                .anchor(Anchor::TopLeft)
                .pivot(Anchor::TopLeft)
                .color(color::rgb(0.7, 0.2, 0.1))
                .depth(1.0)
                .draw();

            frame.text("assets/font.ttf")
                .text("Crates")
                .position(Vec2::new(155.0, 0.0))
                .scale(20.0)
                .pivot(Anchor::TopLeft)
                .anchor(Anchor::TopLeft)
                .depth(1.1)
                .draw();

            for (i, (_, texture)) in self.level.spawners[spawner].crates.iter().enumerate() {
                frame.image(texture)
                    .pixel_scale(1.0)
                    .position(Vec2::new(162.5, i as f32 * -32.0 - 25.0))
                    .anchor(Anchor::TopLeft)
                    .pivot(Anchor::TopLeft)
                    .depth(1.1)
                    .draw();
            }

            self.selected_crate.map(|c| {
                let mut transform = Transform::new();
                transform.position = Vec2::new(50.0, 100.0);

                self.crate_menu.draw(frame, transform, &self.level.spawners[spawner].crates[c]);
            });
        });
    }

    pub fn update_spawner_menu(&mut self, data: &StateData) {
        if !self.show_spawners_menu {
            return;
        }

        // FIXME: when fumarole gets update to colerate mouseposition correctly with frame size, change this
        let mouse_position = data.mouse_position;

        // select spawner
        if mouse_position.x > -167.5 && mouse_position.x < -107.5 && data.mouse_pressed(MouseButton::Left) {
            let spawner = 75.0 - mouse_position.y;

            if spawner % 12.5 <= 10.0 {
                let spawner = (spawner / 12.5).floor() as i32;

                if spawner >= 0 && spawner < self.level.spawners.len() as i32 {
                    self.selected_spawner = Some(spawner as usize);

                    self.spawner_menu = SPAWNER_MENU;

                    

                    println!("selected spawner: {}", spawner);
                }
            }
        }

        // select crate
        self.selected_spawner.map(|spawner| {
            if mouse_position.x > 0.0 && mouse_position.x < 48.0 && data.mouse_pressed(MouseButton::Left) {
                let _crate = 75.0 - mouse_position.y;

                if _crate % 32.0 <= 32.0 {
                    let _crate = (_crate / 32.0).floor() as i32;

                    if _crate >= 0 && _crate < self.level.spawners[spawner].crates.len() as i32 {
                        self.selected_crate = Some(_crate as usize);

                        self.crate_menu = CRATE_MENU;

                        

                        println!("selected crate: {}", _crate);
                    }
                }
        }});

        // deselect crate
        if data.key_pressed(KeyCode::Escape) {
            self.selected_crate = None;
        }
    }
}

impl State for LevelEditor {
    fn draw(&self, frame: &mut Frame, _data: &StateData) {
        self.level.draw(frame);

        self.draw_spawner_menu(frame);

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
                            .color(spawner.color)
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
                    .color(spawner.color)
                    .depth(1.0)
                    .draw();
            }
        }
    }

    fn update(&mut self, data: &StateData) -> Transition {
        self.update_spawner_menu(data);

        if self.show_spawners_menu {
            self.selected_spawner.map(|spawner| {
                if let Some(_crate) = self.selected_crate {
                    self.crate_menu.update(data, &mut self.level.spawners[spawner].crates[_crate]);
                } else {
                    self.spawner_menu.update(data, &mut self.level.spawners[spawner]);
                }
            });
        }

        // add spawner
        if data.key_pressed(KeyCode::A) && self.show_spawners_menu {
            self.level.spawners.push(
                Spawner {
                    time: 0.0,
                    max_time: 2.0,
                    color: color::rgb(self.random.range_f32(0.0, 1.0), self.random.range_f32(0.0, 1.0), self.random.range_f32(0.0, 1.0)),
                    crates: Vec::new(),
                    nodes: Vec::new(),
                }
            );
        }

        // toggle spawner menu
        if data.key_pressed(KeyCode::S) {
            self.show_spawners_menu = !self.show_spawners_menu;
        }

        // save level
        if data.key_pressed(KeyCode::Return) {
            self.level.save(self.path.clone());
        }

        // remove path
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

        // initiate path drawing
        if data.mouse_pressed(MouseButton::Left) && !self.show_spawners_menu {
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
                            accepted: None,
                            moving: false,
                            speed: 2.0/3.0,
                        }
                    );

                    if self.level.spawners.len() > 0 {
                        self.level.spawners[self.selected_spawner.unwrap_or(0)].nodes.push(
                            (100, index)
                        );
                    }
     
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

        // continue path drawing
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
                                accepted: None,
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

        // stop path drawing
        if data.mouse_released(MouseButton::Left) {
            self.selected_node = None;
        }
        
        Transition::None
    }
}
