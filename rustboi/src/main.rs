#![allow(dead_code)]
mod bus;
mod cpu;
mod gameboi;
mod ppu;
use crate::gameboi::GameBoi;

fn main() {
    let mut rustboi = GameBoi::new();
    rustboi.load_rom_from_path("gb-test-roms/cpu_instrs/individual/01-special.gb");
    loop {
        rustboi.step();
    }
}
