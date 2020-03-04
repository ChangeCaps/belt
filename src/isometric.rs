use fumarole::Vec2;

pub trait FromIso {
    fn from_iso(self) -> Vec2<f32>;
}

pub trait IntoIso {
    fn into_iso(self) -> Vec2<i32>;
}

impl IntoIso for Vec2<f32> {
    fn into_iso(self) -> Vec2<i32> {
        Vec2::new(
            (self.x / 48.0 - self.y / 24.0).round() as i32,
            (self.y / 24.0 + self.x / 48.0).round() as i32,
        )
    }
}

impl FromIso for Vec2<f32> {
    fn from_iso(self) -> Vec2<f32> {
        Vec2::new(
            self.y * 24.0 + self.x * 24.0,
            self.y * 12.0 - self.x * 12.0,
        )
    }
}

impl FromIso for Vec2<i32> {
    fn from_iso(self) -> Vec2<f32> {
        Vec2::new(
            self.y as f32 * 24.0 + self.x as f32 * 24.0,
            self.y as f32 * 12.0 - self.x as f32 * 12.0,
        )
    }
}
