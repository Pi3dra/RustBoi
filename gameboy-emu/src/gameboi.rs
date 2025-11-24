use crate::bus::Bus;
use crate::cpu::CPU;
use crate::ppu::PPU;
use std::cell::RefCell;
use std::rc::Rc;
const IF: u16 = 0xFF0F;
const IE: u16 = 0xFFFF; //Interrupt enable

pub struct GameBoi {
    cpu: CPU,
    ppu: PPU,
    bus: Rc<RefCell<Bus>>,
}

impl GameBoi {
    pub fn new() -> Self {
        let bus = Bus::empty();
        let ppu = PPU::new(bus.clone());
        let cpu = CPU::new(bus.clone());
        Self { cpu, ppu, bus }
    }

    pub fn load_rom_from_path(&mut self, rom_path: &str) {
        self.bus.borrow_mut().load_rom(rom_path);
    }

    pub fn load_rom_from_data(&mut self, rom_data: &[u8]) {
        self.bus.borrow_mut().load_rom_data(rom_data);
    }

    pub fn receive_input(&mut self, pressed_mask: u8) {
        let mut bus = self.bus.borrow_mut();
        let current_select = bus.read(0xFF00, false);

        /*
        println!(
            "INPUT mask {:0b} 0xFF00 : {:0b} IE : {:0b} IF : {:0b}",
            pressed_mask,
            bus.read(0xFF00, false),
            bus.read(IE, false),
            bus.read(IF, false)
        );*/
        // Combine: keep select lines (bits 4-5), apply pressed buttons (bits 0-3)
        let joyp = (current_select & 0x30) | (pressed_mask & 0x0F);

        let old = bus.read(0xFF00, false);
        bus.write(0xFF00, joyp, false);

        // Trigger joypad interrupt on any falling edge in lower 4 bits
        if (old & 0x0F) == 0x0F && (joyp & 0x0F) != 0x0F {
            let if_reg = bus.read(IF, false);
            bus.write(IF, if_reg | 0x10, false);
            //let ie = bus.read(IE, false);
            //bus.write(IE, 0b10000 | ie, false);

            println!(
                "INPUT INTERRUPT SET {:0b} 0xFF00 : {:0b} IE : {:0b} IF : {:0b}",
                pressed_mask,
                bus.read(0xFF00, false),
                bus.read(IE, false),
                bus.read(IF, false)
            );
        }
    }

    pub fn step(&mut self) -> [u8; 23040] {
        while !self.ppu.is_frame_ready() {
            let cycles = self.cpu.step();
            //self.cpu.print_state();
            self.ppu.step(cycles / 2);
            //ppu.print_state();
        }
        let frame = self.ppu.yield_frame();
        self.ppu.clear_buffer();
        frame
    }
}
