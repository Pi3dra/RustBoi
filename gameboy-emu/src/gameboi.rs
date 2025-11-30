use crate::bus::Bus;
use crate::cpu::CPU;
use crate::ppu::PPU;
use std::cell::RefCell;
use std::rc::Rc;
const IF: u16 = 0xFF0F;
//const IE: u16 = 0xFFFF;

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
        let trigger_interrupt = bus.set_joypad(pressed_mask);

        if trigger_interrupt {
            let if_reg = bus.read(IF, false);
            bus.write(IF, if_reg | 0x10, false);
        }
    }

    pub fn step(&mut self) -> [u8; 23040] {
        while !self.ppu.is_frame_ready() {
            let cycles = self.cpu.step();
            //self.cpu.print_state();
            //TODO THIS SHOULD BE 4 BUT I AM GETTING OVERFLOWS
            //Interesting bug info
            //buttontest.gb = *4 completely breaks it and doesn't show anymore
            //boxxle.gb = *4 show a bugged intro animation instead of blank
            //
            self.ppu.step(cycles * 4); 
            //ppu.print_state();
        }
        let frame = self.ppu.yield_frame();
        self.ppu.clear_buffer();
        frame
    }
}
