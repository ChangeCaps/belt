use fumarole::*;
use std::collections::HashMap;
use crate::isometric::*;
use serde::{Serialize, Deserialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct Belt {
    pub position: Vec2<i32>,
    pub texture: &'static str,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Hole {
    pub position: Vec2<i32>,
    pub back_texture: &'static str,
    pub front_texture: &'static str,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Tile {
    pub texture: &'static str,
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
    pub paths: Vec<(i32, usize)>,
}

#[derive(Clone, Serialize)]
pub struct Level {
    pub belts: Vec<Belt>,
    pub holes: Vec<Hole>,
    pub tiles: HashMap<(i32, i32), Tile>,
    pub nodes: Vec<Node>,
}

impl Level {
    pub fn load(path: String) -> Self {
        Level {
            belts: Vec::new(),
            holes: Vec::new(),
            tiles: HashMap::new(),
            nodes: Vec::new(),
        }
    }

    pub fn draw(&self, frame: &mut Frame) {
        // draw belts
        for belt in &self.belts {
            frame.image(belt.texture)
                .position(belt.position.from_iso() - Vec2::new(2.0, 3.0))
                .depth(-belt.position.from_iso().y / 1000.0 - 0.011)
                .pixel_scale(1.0001)
                .draw();
        }

        // draw holes
        for hole in &self.holes {
            frame.image(hole.back_texture)
                .position(hole.position.from_iso() - Vec2::new(0.0, 7.0))
                .depth(-hole.position.from_iso().y / 1000.0 - 0.006)
                .pixel_scale(1.0001)
                .draw();

            frame.image(hole.front_texture)
                .position(hole.position.from_iso() - Vec2::new(0.0, 7.0))
                .depth(-hole.position.from_iso().y / 1000.0 + std::f32::EPSILON)
                .pixel_scale(1.0001)
                .draw();
        }

        // draw tiles
        for (position, tile) in &self.tiles {
            let position = Vec2::new(position.0, position.1).from_iso();

            frame.image(tile.texture)
                .position(position - Vec2::new(0.0, 7.0))
                .depth(-position.y / 1000.0 - 0.01)
                .pixel_scale(1.0001)
                .draw();
        }
    }
}