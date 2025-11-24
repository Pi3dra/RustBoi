use libretro_rs::{
    RetroAudioInfo, RetroCore, RetroEnvironment, RetroGame, RetroJoypadButton, RetroLoadGameResult,
    RetroRuntime, RetroSystemInfo, RetroVideoInfo, libretro_core,
};

mod bus;
mod cpu;
mod gameboi;
mod ppu;
use crate::gameboi::*;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;

const DMG_PALETTE: [(u8, u8, u8); 4] = [
    (155, 188, 15), // lightest
    (139, 172, 15),
    (48, 98, 48),
    (15, 56, 15), // darkest
];

fn dmg_to_rgb565(level: u8) -> u16 {
    // Clamp to 0..=3 — this prevents the panic NO MATTER WHAT the PPU writes
    let idx = (level as usize) & 0b11; // equivalent to level % 4, but faster and safe
    let (r, g, b) = DMG_PALETTE[idx];

    ((r as u16 >> 3) << 11) | ((g as u16 >> 2) << 5) | (b as u16 >> 3)
}
/*

Implementing the libretro backend to easily implement a fronted!

https://crates.io/crates/libretro-backend
https://www.libretro.com/index.php/api/

Run like so :  retroarch --verbose -L ./target/debug/libgameboy_emu.so ../cpu_instrs.gb
*/

struct RustBoiCore {
    framebuffer: [u16; WIDTH * HEIGHT],
    gameboi: GameBoi,
}

use RetroJoypadButton::*;
impl RetroCore for RustBoiCore {
    fn init(_env: &RetroEnvironment) -> Self {
        let mut core = Self {
            framebuffer: [0; WIDTH * HEIGHT],
            gameboi: GameBoi::new(),
        };
        println!("INIT!");
        core.gameboi.load_rom_from_path("dmg-acid2.gb");

        /*
        // Fill background with the lightest DMG color
        let light = dmg_to_rgb565(3);
        core.framebuffer.fill(light);

        // Draw a dark square (for testing)
        let dark = dmg_to_rgb565(0);
        let square_size = 64;
        let start_x = (WIDTH - square_size) / 2;
        let start_y = (HEIGHT - square_size) / 2;

        for y in start_y..(start_y + square_size) {
            for x in start_x..(start_x + square_size) {
                core.framebuffer[y * WIDTH + x] = dark;
            }
        }
        */
        core
    }

    fn get_system_info() -> RetroSystemInfo {
        RetroSystemInfo::new("RustBoi", "1.0").with_valid_extensions(&["gb", "gbc", ".gb", ".gbc"])
    }

    fn reset(&mut self, _env: &RetroEnvironment) {
        self.framebuffer = [0xFF; WIDTH * HEIGHT];
        self.gameboi = GameBoi::new();
    }
    fn run(&mut self, _env: &RetroEnvironment, runtime: &RetroRuntime) {
        let mut pressed = 0xFF;

        // Set bits for pressed buttons (bit = pressed)
        if runtime.is_joypad_button_pressed(0, Right) || runtime.is_joypad_button_pressed(0, A) {
            pressed &= !0x01;
        }
        if runtime.is_joypad_button_pressed(0, Left) || runtime.is_joypad_button_pressed(0, B) {
            pressed &= !0x02;
        }
        if runtime.is_joypad_button_pressed(0, Up) || runtime.is_joypad_button_pressed(0, Select) {
            pressed &= !0x04;
        }
        if runtime.is_joypad_button_pressed(0, Down) || runtime.is_joypad_button_pressed(0, Start) {
            pressed &= !0x08;
        }

        self.gameboi.receive_input(pressed);

        // Run one full frame → you get [u8; 23040] of color indices (0-3)
        let raw_frame: [u8; WIDTH * HEIGHT] = self.gameboi.step();

        // Convert DMG color index (0-3) → RGB565 u16
        for (i, &color_index) in raw_frame.iter().enumerate() {
            self.framebuffer[i] = dmg_to_rgb565(color_index);
        }

        // SAFETY: &[u16] has the same memory layout as &[u8] with double the length
        // This is safe because u16 has no padding and alignment is fine on all platforms
        let bytes: &[u8] = unsafe {
            std::slice::from_raw_parts(
                self.framebuffer.as_ptr() as *const u8,
                self.framebuffer.len() * std::mem::size_of::<u16>(),
            )
        };

        // Now upload as raw bytes with correct pitch
        runtime.upload_video_frame(bytes, WIDTH as u32, HEIGHT as u32, WIDTH * 2);
    }

    fn load_game(&mut self, _env: &RetroEnvironment, game: RetroGame) -> RetroLoadGameResult {
        println!("LOADING!");
        match game {
            RetroGame::Path { path, .. } => {
                println!("Path!");
                self.gameboi.load_rom_from_path(path)
            }
            RetroGame::Data { data, .. } => {
                println!("Data!");
                self.gameboi.load_rom_from_data(data)
            }
            RetroGame::None { .. } => panic!(),
        }

        let video = RetroVideoInfo::new(
            59.7275, // GB framerate
            WIDTH as u32,
            HEIGHT as u32,
        )
        .with_pixel_format(libretro_rs::RetroPixelFormat::RGB565);

        let audio = RetroAudioInfo::new(44100.0);

        RetroLoadGameResult::Success { audio, video }
    }
}

libretro_core!(RustBoiCore);
