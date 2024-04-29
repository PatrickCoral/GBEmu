mod mmu;
mod cpu;
mod parser;

use std::time::{Duration, Instant};

use mmu::MMU;

struct Timer {
    curr_time: Instant,
    prev_time: Instant,
    framte_time: Duration,
}

impl Timer {
    fn init() -> Self {
        let time = Instant::now();
        Self {
            curr_time: time,
            prev_time: time,
            framte_time: Duration::from_millis((1.0/59.73 * 1000.0) as u64),
        }
    }

    fn delta_time(&mut self) -> Duration {
        self.prev_time = self.curr_time;
        self.curr_time = Instant::now();
        self.curr_time.duration_since(self.prev_time)
    }
}

fn main() {
    let mmu = MMU::new();
}
