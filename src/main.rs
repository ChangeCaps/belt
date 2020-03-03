extern crate fumarole;

use fumarole::*;
use std::io::Read;

pub mod states;
pub mod isometric;
pub mod language;
pub mod random;

fn main() {
    let mut code = String::new();
    
    std::fs::File::open("code.belt").unwrap().read_to_string(&mut code).unwrap();

    fumarole::Application::new()
        .with_window_size(1065, 600)
        .with_frame_size(355.0, 200.0)
        .with_depth_sorting(true)
        .run(|loader| {
            use states::*;

            loader.load_image_from_raw(include_bytes!("../assets/claw_closed.png"), PNG, "claw_closed");
            loader.load_image_from_raw(include_bytes!("../assets/claw_open.png"), PNG, "claw_open");
            loader.load_image_from_raw(include_bytes!("../assets/claw_back_closed.png"), PNG, "claw_back_closed");
            loader.load_image_from_raw(include_bytes!("../assets/claw_back_open.png"), PNG, "claw_back_open");
            loader.load_image_from_raw(include_bytes!("../assets/claw_pole.png"), PNG, "claw_pole");

            loader.load_image_from_raw(include_bytes!("../assets/hole_back_blue.png"), PNG, "hole_back_blue");
            loader.load_image_from_raw(include_bytes!("../assets/hole_front_blue.png"), PNG, "hole_front_blue");

            loader.load_image_from_raw(include_bytes!("../assets/floor.png"), PNG, "floor");

            loader.load_image_from_raw(include_bytes!("../assets/crate_0.png"), PNG, "crate_0");
            loader.load_image_from_raw(include_bytes!("../assets/crate_1.png"), PNG, "crate_1");

            loader.load_image_from_raw(include_bytes!("../assets/belt.png"), PNG, "belt");

            Box::new(
                states::Game::new(
                    code.clone(),
                )
            )
        });
}
