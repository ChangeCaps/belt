// an implementation of PRNG
#[derive(Clone, Debug)]
pub struct Random {
    pub x: u32,
    pub modulus: u32,
    pub multiplier: u32,
    pub increment: u32,
}

impl Random {
    pub fn new(seed: u32) -> Self {
        Random {
            x: seed + 120121,
            modulus: 2147483647,
            multiplier: 48271,
            increment: 0,
        }
    }

    pub fn next(&mut self) -> u32 {
        let output = (self.multiplier * self.x + self.increment) % self.modulus;
        self.x = output;
        output
    } 

    pub fn range_i32(&mut self, min: i32, max: i32) -> i32 {
        let min = min.min(max);
        let range = (max - min).abs() + 1;

        self.next() as i32 % range + min
    }
    
    pub fn range_usize(&mut self, min: usize, max: usize) -> usize {
        let min = min.min(max);
        let range = (max as i32 - min as i32).abs() as usize + 1;

        self.next() as usize % range + min
    }

    pub fn range_f32(&mut self, min: f32, max: f32) -> f32 {
        let min = min.min(max);
        let range = (max - min).abs();

        self.next() as f32 / (self.modulus as f32 / range) + min
    }
}
