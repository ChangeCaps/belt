use fumarole::*;
use std::collections::HashMap;
use crate::isometric::*;
use serde::{Serialize, Deserialize};
use std::io::prelude::*;

#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum Tile {
    Floor(String),
    Belt(String),
    Hole(String, String),
}

impl Tile {
    pub fn draw(&self, frame: &mut Frame, position: Vec2<f32>) {
        match self {
            Tile::Floor(texture) => {
                frame.image(texture)
                    .position(position - Vec2::new(0.0, 0.0))
                    .depth(-position.y / 1000.0 - 0.01)
                    .pixel_scale(1.0001)
                    .draw();
            },
            Tile::Belt(texture) => {
                frame.image(texture)
                    .position(position - Vec2::new(2.0, 3.0 - self.height()))
                    .depth(-position.y / 1000.0 - 0.011)
                    .pixel_scale(1.0001)
                    .draw();
            },
            Tile::Hole(back_texture, front_texture) => {
                frame.image(back_texture)
                    .position(position - Vec2::new(0.0, 0.0))
                    .depth(-position.y / 1000.0 - 0.006)
                    .pixel_scale(1.0001)
                    .draw();
    
                frame.image(front_texture)
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

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Spawner {
    pub time: f32,
    pub max_time: f32,
    pub crates: Vec<(i32, String)>,
    pub nodes: Vec<(i32, usize)>,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
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
                tiles.insert((x, y), Tile::Floor("floor".to_string()));
            }
        }

        Level {
            tiles,
            nodes: Vec::new(),
            spawners: Vec::new(),
        }
    }

    pub fn save(&self, path: String) {
        let mut file = std::fs::File::create("level0.lvl").expect("failed to load file");

        file.write(&bincode::serialize(self).unwrap()).unwrap();
    }

    pub fn load(path: String) -> Self {
        let mut file = std::fs::File::open(path).unwrap();

        let mut buf = Vec::new();

        file.read_to_end(&mut buf).unwrap();

        bincode::deserialize(&buf).unwrap()
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
            if let Some(next) = self.nodes[node].next {
                if self.nodes[next].next.is_none() && next == target_node {
                    self.nodes[node].next = None;
                }

                if next > target_node {
                    self.nodes[node].next = Some(next - 1);
                }
            } 
        }

        for spawner in &mut self.spawners {
            let mut node = 0;

            while node < spawner.nodes.len() {
                if spawner.nodes[node].1 == target_node {
                    spawner.nodes.swap_remove(node);
                    continue;
                }

                if spawner.nodes[node].1 > target_node {
                    spawner.nodes[node].1 -= 1;
                }

                node += 1;
            }
        }

        *self.get_tile_mut(self.nodes[target_node].position).unwrap() = Tile::Floor("floor".to_string());

        self.nodes.remove(target_node);
    }

    pub fn remove_path(&mut self, node: usize) {
        self.nodes[node].next.map(|node| self.remove_path(node));
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
