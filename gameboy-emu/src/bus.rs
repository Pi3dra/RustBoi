use crate::ppu::StatRegister;
use crate::ppu::State;
use std::cell::RefCell;
use std::rc::Rc;
const DMA: u16 = 0xFF46;
const STAT: u16 = 0xFF41;

pub struct Bus {
    memory: Memory,
}

/*
Currently we only need to separate CPU and PPU acces so a bool is enough,
but might change to an enum later on?
*/

pub trait BusAccess {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, value: u8);
}

//TODO: Handle CPU blocking depending on PPU state
impl Bus {
    pub fn empty() -> Rc<RefCell<Self>> {
        let memory = Memory::new(vec![]);
        Rc::new(RefCell::new(Self { memory }))
    }

    //This loads from a path
    pub fn load_rom(&mut self, path: &str) {
        let rom = std::fs::read(path).expect("Failed to read ROM file");
        self.memory.load_rom(&rom);
    }

    //This loads already loaded data
    pub fn load_rom_data(&mut self, data: &[u8]) {
        self.memory.load_rom(data);
    }

    pub fn write(&mut self, address: u16, value: u8, cpuread: bool) {
        //This breaks loading for some reason
        /*
        if cpuread && self.cpu_can_acces(address){
            self.memory.write(address, value);
        } else if !cpuread {
            self.memory.write(address, value);
        }*/

        self.memory.write(address, value);

        //Handling DMA Transfer
        if address == DMA {
            let mut new_oam = [0; 160];
            let base_address = (value as u16) << 8;
            for i in 0..160 {
                new_oam[i] = self.memory.read(base_address + i as u16);
            }
            self.memory.oam = new_oam;
            //TODO: Advance cpu clock by 160 M Cycles
            //Note this is not cycle accurate, normally the dma transfer is done by parts, in which
            //the cpu can either be executing nops or small procedures in HRAM
        }
    }

    pub fn read(&mut self, address: u16, cpuread: bool) -> u8 {
        if cpuread && !self.cpu_can_acces(address) {
            0xFF
        } else {
            if address == 0xFF00 {
                return self.read_joyp();
            }
            self.memory.read(address)
        }
    }

    fn read_joyp(&mut self) -> u8 {
        let ff0 = self.memory.read(0xFF00 ) & 0b1111_1011;
        println!("ff0 {:0b}, result {:0b}", ff0, ff0 & 0b1111_1011 | 0b0011_0111);
        ff0 & 0b1111_1011 | 0b0010_0111
    }

    fn get_ppu_state(&mut self) -> State {
        StatRegister::new(self.read(STAT, false)).get_ppu_state()
    }

    fn cpu_can_acces(&mut self, address: u16) -> bool {
        match self.get_ppu_state() {
            State::OAMSearch => !(0xFE00..=0xFE9F).contains(&address),

            State::PixelTransfer => !(0x8000..=0x9FFF).contains(&address),

            State::HBlank | State::VBlank => true,

            _ => unreachable!(),
        }
    }
}

struct Memory {
    rom0: [u8; 16_384],
    romn: [u8; 16_384],

    vram: [u8; 8_192],
    ram: [u8; 8_192],

    wram1: [u8; 4_096],
    wram2: [u8; 4_096],
    hram: [u8; 127],

    oam: [u8; 160],

    io: [u8; 128],
    interrupt: [u8; 1],
}

impl Memory {
    pub fn new(rom: Vec<u8>) -> Self {
        // Split ROM into 0x0000–0x3FFF (bank 0) and 0x4000–0x7FFF (bank 1)
        let mut rom0 = [0u8; 0x4000];
        let mut romn = [0u8; 0x4000];

        for (i, byte) in rom.iter().enumerate() {
            if i < 0x4000 {
                rom0[i] = *byte;
            } else if i < 0x8000 {
                romn[i - 0x4000] = *byte;
            } else {
                break; // ignore any extra bytes (Game Boy ROM limit per bank)
            }
        }

        Self {
            rom0,
            romn,
            vram: [0; 0x2000],
            ram: [0; 0x2000],
            wram1: [0; 0x1000],
            wram2: [0; 0x1000],
            oam: [0; 0xA0],
            io: [0; 0x80],
            hram: [0; 0x7F],
            interrupt: [0; 1],
        }
    }

    fn handle_blarg_output(&mut self, address: u16, value: u8) {
        if address == 0xFF02 && value == 0x81 {
            let c = self.io[0x01] as char; // Read the byte to send
            print!("{}", c); // Print immediately
            std::io::Write::flush(&mut std::io::stdout()).unwrap();
            self.io[0x02] = 0; // Clear "transfer in progress" flag
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        // Handle serial transfer for Blargg tests
        self.handle_blarg_output(address, value);
        let (region, address, _, writable) = self.map(address);

        if address == 0xFFFF {
            println!("Writing to IE {}", value);
        }

        if writable {
            region[address] = value;
        }
    }

    fn read(&mut self, address: u16) -> u8 {
        let (region, address, readable, _) = self.map(address);
        if readable {
            return region[address];
        }

        0xFF
    }

    fn load_rom(&mut self, rom: &[u8]) {
        for (i, byte) in rom.iter().enumerate() {
            if i < 0x4000 {
                self.rom0[i] = *byte;
            } else if i < 0x8000 {
                self.romn[i - 0x4000] = *byte;
            } else {
                break;
            }
        }
    }

    fn map(&mut self, address: u16) -> (&mut [u8], usize, bool, bool) {
        match address {
            0x0000..=0x3FFF => (&mut self.rom0, address as usize, true, false),
            0x4000..=0x7FFF => (&mut self.romn, (address - 0x4000) as usize, true, false),

            0x8000..=0x9FFF => (&mut self.vram, (address - 0x8000) as usize, true, true),
            0xA000..=0xBFFF => (&mut self.ram, (address - 0xA000) as usize, true, true),

            0xC000..=0xCFFF => (&mut self.wram1, (address - 0xC000) as usize, true, true),
            0xD000..=0xDFFF => (&mut self.wram2, (address - 0xD000) as usize, true, true),

            // Echo RAM mirrors C000–DDFF
            0xE000..=0xEFFF => (&mut self.wram1, (address - 0xE000) as usize, true, true),
            0xF000..=0xFDFF => (&mut self.wram2, (address - 0xF000) as usize, true, true),

            0xFE00..=0xFE9F => (&mut self.oam, (address - 0xFE00) as usize, true, true),
            0xFEA0..=0xFEFF => (&mut self.oam, 0, false, false), // not usable; return dummy

            0xFF00..=0xFF7F => (&mut self.io, (address - 0xFF00) as usize, true, true),
            0xFF80..=0xFFFE => (&mut self.hram, (address - 0xFF80) as usize, true, true),

            0xFFFF => (&mut self.interrupt, 0, true, true),
        }
    }
}
