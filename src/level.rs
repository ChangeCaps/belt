use fumarole::*;
use std::collections::HashMap;
use crate::isometric::*;
use serde::{Serialize, Deserialize};
use std::io::Read;

#[derive(Clone, Serialize, Deserialize)]
pub enum Tile {
    Floor(&'static str),
    Belt(&'static str),
    Hole(&'static str, &'static str),
}

impl Tile {
    pub fn draw(&self, frame: &mut Frame, position: Vec2<f32>) {
        match self {
            Tile::Floor(texture) => {
                frame.image(*texture)
                    .position(position - Vec2::new(0.0, 0.0))
                    .depth(-position.y / 1000.0 - 0.01)
                    .pixel_scale(1.0001)
                    .draw();
            },
            Tile::Belt(texture) => {
                frame.image(*texture)
                    .position(position - Vec2::new(2.0, 3.0 - self.height()))
                    .depth(-position.y / 1000.0 - 0.011)
                    .pixel_scale(1.0001)
                    .draw();
            },
            Tile::Hole(back_texture, front_texture) => {
                frame.image(*back_texture)
                    .position(position - Vec2::new(0.0, 0.0))
                    .depth(-position.y / 1000.0 - 0.006)
                    .pixel_scale(1.0001)
                    .draw();
    
                frame.image(*front_texture)
                    .position(position - Vec2::new(0.0, 0.0))
                    .depth(-position.y / 1000.0 + std::f32::EPSILON)
                    .pixel_scale(1.0001)
                    .draw();
            }
        }
    }

    pub fn height(&self) -> f32 {
        match self {
            Tile::Belt(_) => 7.0,
            _ => 0.0,
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Node {
    pub position: Vec2<i32>,
    pub occupant: Option<usize>,
    pub next: Option<usize>,
    pub moving: bool,
    pub speed: f32,
}

#[derive(Clone, Serialize)]
pub struct Spawner {
    pub time: f32,
    pub max_time: f32,
    pub crates: Vec<(i32, &'static str)>,
    pub nodes: Vec<(i32, usize)>,
}

#[derive(Clone, Serialize)]
pub struct Level { 
    pub tiles: HashMap<(i32, i32), Tile>,
    pub nodes: Vec<Node>,
    pub spawners: Vec<Spawner>,
}

impl Level {
    pub fn new() -> Self {
        let mut tiles = HashMap::new();
        
        for x in -10..10 {
            for y in -10..10 {
                tiles.insert((x, y), Tile::Floor("floor"));
            }
        }

        Level {
            tiles,
            nodes: Vec::new(),
            spawners: Vec::new(),
        }
    }

    pub fn load(path: String) -> Self {
        let mut file = std::fs::File::open(path).unwrap();

        let mut buf = Vec::new();

        file.read_to_end(&mut buf).unwrap();

        Level {
            tiles: HashMap::new(),
            nodes: Vec::new(),
            spawners: Vec::new(),
        }
    }

    pub fn get_height(&self, position: Vec2<i32>) -> f32 {
        self.tiles.get(&(position.x, position.y)).map(|tile| tile.height()).unwrap_or(0.0)
    }

    pub fn get_tile(&self, position: Vec2<i32>) -> Option<&Tile> {
        self.tiles.get(&(position.x, position.y))
    }

    pub fn get_tile_mut(&mut self, position: Vec2<i32>) -> Option<&mut Tile>  {
        self.tiles.get_mut(&(position.x, position.y))
    }

    pub fn remove_node(&mut self, target_node: usize) {
        for node in 0..self.nodes.len() {
            if self.nodes[node].next == Some(target_node) && self.nodes[target_node].next.is_none() {
                self.nodes[node].next = None;
            }

            self.nodes[node].next.as_mut().map(|next| {
                if *next > target_node {
                    *next -= 1;
                }
            }); 
        }

        *self.get_tile_mut(self.nodes[target_node].position).unwrap() = Tile::Floor("floor");

        self.nodes.remove(target_node);
    }

    pub fn remove_path(&mut self, mut node: usize) {
        while let Some(next) = self.nodes[node].next {
            self.remove_node(node);

            if next > node {
                node = next - 1;
            } else {
                node = next;
            }
        }

        self.remove_node(node);
    }

    pub fn draw(&self, frame: &mut Frame) {
        // draw tiles
        for (position, tile) in &self.tiles {
            let position = Vec2::new(position.0, position.1).from_iso(); 

            tile.draw(frame, position);
        }
    }
}
