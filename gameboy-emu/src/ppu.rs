//LCD controlppu
#![allow(unused_variables, unused, dead_code)]
const LCDC: u16 = 0xFF40;
const STAT: u16 = 0xFF41; //Scrolling and misc
const SCY: u16 = 0xFF42;
const SCX: u16 = 0xFF43;
const LY: u16 = 0xFF44;
const LYC: u16 = 0xFF45;
//Palletes
const BGP: u16 = 0xFF47;
const OBP0: u16 = 0xFF48;
const OBP1: u16 = 0xFF49;
//Window position
const WY: u16 = 0xFF4A;
const WX: u16 = 0xFF4B;
//For requesting interrupts
const IF: u16 = 0xFF0F;
const INT_VBLANK: u8 = 0;
const INT_STAT: u8 = 1;

//Important Addresses
const TILE_MAP0_ADDRESS: u16 = 0x9800; // To 0x9BFF
const TILE_MAP1_ADDRESS: u16 = 0x9C00; // To 0x9BFF
const TILE_DATA_BASE_UNSIGNED: u16 = 0x8000;
const TILE_DATA_BASE_SIGNED: u16 = 0x9000;
const OAM: u16 = 0xFE00; // TO 0xFE9F

use crate::bus::Bus;
use crate::bus::BusAccess;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;

enum TileIndexing {
    Unsigned,
    Signed,
}

// ========== Bus acces for both PPU, and PixelFetcher ==========

macro_rules! impl_bus_access {
    ($t:ty) => {
        impl BusAccess for $t {
            fn read(&self, addr: u16) -> u8 {
                self.bus.borrow_mut().read(addr, false)
            }

            fn write(&mut self, addr: u16, value: u8) {
                self.bus.borrow_mut().write(addr, value, false);
            }
        }
    };
}

impl_bus_access!(PPU);
impl_bus_access!(PixelFetcher);

// ========== Important registers ==========

struct LcdcRegister {
    ppu_enabled: bool,
    window_tilemap: bool, // 0->0x9800-0x9BFF 1->0x9C00-0x9FFF
    window_enabled: bool,
    bg_window_tiles: bool, // 0->0x8800-0x97FF 1->0x8000-0x8FFF
    bg_tilemap: bool,      // 0->0x9800-0x9BFF 1->0x9C00-0x9FFF
    obj_size: bool,
    obj_enable: bool,
    bg_enable: bool,
}

impl LcdcRegister {
    fn new(register: u8) -> Self {
        Self {
            ppu_enabled: register & 0x80 != 0,     // Bit 7
            window_tilemap: register & 0x40 != 0,  // Bit 6
            window_enabled: register & 0x20 != 0,  // Bit 5
            bg_window_tiles: register & 0x10 != 0, // Bit 4
            bg_tilemap: register & 0x08 != 0,      // Bit 3
            obj_size: register & 0x04 != 0,        // 0 -> 8 , 1 -> 16
            obj_enable: register & 0x02 != 0,      // Bit 1
            bg_enable: register & 0x01 != 0,       // Bit 0
        }
    }
}

pub struct StatRegister {
    lyc_select: bool,
    mode2: bool,
    mode1: bool,
    mode0: bool,
    lyc_compare: bool,
    ppu_state: u8,
}

impl StatRegister {
    pub fn new(register: u8) -> Self {
        Self {
            //These Bits allows the CPU, to tell the PPU, when to enable a STAT interrupt!
            lyc_select: register & 0x40 != 0,
            mode2: register & 0x20 != 0, // -> Interrupt on OAM Search
            mode1: register & 0x10 != 0, // -> Interrupt on VBlank
            mode0: register & 0x08 != 0, // -> Interrupt on HBlank
            // Other flags
            lyc_compare: register & 0x04 != 0, //  LY==LYC
            ppu_state: register & 0x03,        // 0: HBlank  1:VBlank 2:OAM 3:Drawing
        }
    }

    pub fn get_ppu_state(&self) -> State {
        match self.ppu_state {
            0 => HBlank,
            1 => VBlank,
            2 => OAMSearch,
            3 => PixelTransfer,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Obj {
    x: u8,
    y: u8,
    tile_index: u8,
    priority: u8,
    flipx: bool,
    flipy: bool,
    palette: u8,
}

impl Obj {
    fn default() -> Obj {
        Obj {
            x: 0,
            y: 0,
            tile_index: 0,
            priority: 0,
            flipx: false,
            flipy: false,
            palette: 0,
        }
    }
}

// ============ PPU ============

type TileBytes = [u8; 16];
type Tile = [[u8; 8]; 8];
type TileMapIndexed = [[u8; 32]; 32];
type TileMapTiles = [[Tile; 32]; 32];

#[derive(Clone, Debug)]
pub enum State {
    Idle = 4,
    OAMSearch = 2,
    PixelTransfer = 3,
    HBlank = 0,
    VBlank = 1,
}

pub struct PPU {
    bus: Rc<RefCell<Bus>>,
    framebuffer: Option<[u8; WIDTH * HEIGHT]>,
    viewport: [u8; WIDTH * HEIGHT],

    state: State,
    fetcher: PixelFetcher,
    bg_fifo: PixelFIFO,
    obj_fifo: PixelFIFO,

    fine_scroll_x: u8,
    popped_pixels: u16,
    vblank_line_clock: u16,

    clock: u16,
    line_objs: Option<Vec<Obj>>,
}

use State::*;
impl PPU {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        let framebuffer = None;
        let viewport = [0xFF; WIDTH * HEIGHT];
        let state = OAMSearch;
        let fetcher = PixelFetcher::new(bus.clone());
        let bg_fifo = PixelFIFO::new();
        let obj_fifo = PixelFIFO::new();
        let clock = 0;

        //This is getting updated nowhere
        let fine_scroll_x = 0;

        let vblank_line_clock = 0;
        let popped_pixels = 0;
        let line_objs = None;

        let mut ppu = Self {
            bus,
            framebuffer,
            viewport,
            state,
            fetcher,
            bg_fifo,
            obj_fifo,
            fine_scroll_x,
            popped_pixels,
            vblank_line_clock,
            clock,
            line_objs,
        };

        ppu
    }

    pub fn print_state(&self) {
        println!(
            "State {:?} LY {} Clock {}",
            self.state,
            self.read(LY),
            self.clock
        );
    }

    fn fetch_lcdc_register(&self) -> LcdcRegister {
        LcdcRegister::new(self.read(LCDC))
    }

    // ============ Objects & OAMSearch ============

    fn fetch_object(&self, address: u16) -> Obj {
        //Each object is 4 bytes long
        let y = self.read(address); // + 16;
        let x = self.read(address + 1); // + 8;
        let tile_index = self.read(address + 2);
        let flags = self.read(address + 3);

        Obj {
            x: x,
            y: y,
            tile_index: tile_index,
            priority: flags & 0x80,
            flipy: flags & 0x40 != 0,
            flipx: flags & 0x20 != 0,
            palette: (flags & 0x10) >> 4,
        }
    }

    fn fetch_objects_from_oam(&self) -> [Obj; 40] {
        let mut objects = [Obj::default(); 40];

        for i in 0..40 {
            objects[i] = self.fetch_object(OAM + (i as u16) * 4);
        }

        objects
    }

    fn fetch_tile_bytes_unsigned(&self, index: u8) -> TileBytes {
        let mut bytes: [u8; 16] = [0x00; 16];

        // Tile address for unsigned mode (0x8000 base)
        let address: u16 = TILE_DATA_BASE_UNSIGNED
            .checked_add((index as u16) * 16)
            .expect("Unsigned tile address overflow");

        for i in 0..16 {
            bytes[i as usize] = self.read(address + i as u16);
        }

        bytes
    }

    fn oamsearch(&mut self, _cycles: u8) {
        if !matches!(self.line_objs, None) {
            return; // This means oamsearch has already been done, we are just stalling to simulate
            // cycles now
        }

        let lcdc = LcdcRegister::new(self.read(LCDC));
        let sprite_height = { if lcdc.obj_size { 16 } else { 8 } };

        let oam_data = self.fetch_objects_from_oam();
        let ly = self.read(LY);
        let mut objects_to_draw: Vec<Obj> = vec![];

        for object_data in oam_data {
            let sprite_top = object_data.y.wrapping_sub(16);
            let sprite_bottom = sprite_top.wrapping_add(sprite_height);

            if ly >= sprite_top && ly < sprite_bottom {
                if objects_to_draw.len() < 10 {
                    objects_to_draw.push(object_data);
                } else {
                    break;
                }
            }
        }
        objects_to_draw.sort_by_key(|obj| obj.x);
        self.line_objs = Some(objects_to_draw);
    }

    // ============ PixelTransfer ============

    fn apply_palette(&self, mut pixel: Pixel) -> Pixel {
        let palette = match pixel.palette {
            Option::None => self.read(BGP),
            Some(0) => self.read(OBP0),
            Some(1) => self.read(OBP1),
            _ => unreachable!(),
        };
        let shade = ((palette >> (pixel.color * 2)) & 3) as u8;
        pixel.color = shade;
        pixel
    }

    fn objects_at(&mut self, current_x: i32) -> Option<Obj> {
        let ly = self.read(LY);
        let mut overlapping_objects: Vec<Obj> = vec![];

        let pixel_on_screen = current_x >= 0 && current_x < WIDTH as i32;
        if pixel_on_screen {
            for obj in self.line_objs.as_ref().unwrap() {
                // check if any sprite starts at this X
                if (obj.x as i32 - 8) == current_x {
                    // <- this somewhat controls bad stuff
                    overlapping_objects.push(*obj);
                }
            }
        }

        if overlapping_objects.len() > 1 {
            //println!("Overlapping! {:?}", overlapping_objects);
        }
        //TODO: Handle object priority here
        if overlapping_objects.is_empty() {
            None
        } else {
            Some(overlapping_objects[0])
        }
    }

    fn update_obj_fifo(&mut self, obj: Obj) {
        let ly = self.read(LY);
        let sprite_top_y = obj.y.wrapping_sub(16);
        let mut local_y = ly.wrapping_sub(sprite_top_y) as usize;

        let lcdc = self.fetch_lcdc_register();
        let height = if lcdc.obj_size { 16 } else { 8 };

        // Early out if we're outside the sprite
        if local_y >= height {
            return;
        }

        // === Vertical flip ===
        if obj.flipy {
            local_y = (height - 1) - local_y;
        }

        // === Determine which tile and which row inside it ===
        let tile_index = if height == 16 {
            // For 8×16: bit 0 of tile index is ignored, we use it to select top/bottom
            let base = obj.tile_index & 0xFE;
            if local_y >= 8 { base + 1 } else { base }
        } else {
            obj.tile_index
        };

        let row_in_tile = if height == 16 && local_y >= 8 {
            local_y - 8
        } else {
            local_y
        };

        // Fetch the correct 16-byte tile
        let tile_data = self.fetch_tile_bytes_unsigned(tile_index);

        // Get the correct row (0..7)
        let mut low_byte = tile_data[row_in_tile * 2];
        let mut high_byte = tile_data[row_in_tile * 2 + 1];

        // Horizontal flip
        if obj.flipx {
            low_byte = low_byte.reverse_bits();
            high_byte = high_byte.reverse_bits();
        }

        // Push the 8 pixels
        self.obj_fifo
            .push_tile_from_bytes(low_byte, high_byte, Some(obj));
    }

    fn mix_fifo_pixels(&mut self) -> u8 {
        let lcdc = self.fetch_lcdc_register();

        match (self.bg_fifo.pop(), self.obj_fifo.pop()) {
            (Some(bg_pixel), Option::None) => {
                if lcdc.bg_enable {
                    self.apply_palette(bg_pixel).color
                } else {
                    0
                }
            }
            (Some(mut bg_pixel), Some(mut obj_pixel)) => {
                bg_pixel = self.apply_palette(bg_pixel);
                obj_pixel = self.apply_palette(obj_pixel);

                if lcdc.obj_enable {
                    if obj_pixel.color == 0 {
                        bg_pixel.color
                    } else {
                        let bg_is_white = bg_pixel.color == 0;
                        let sprite_in_front = obj_pixel.sprite_priority;
                        let use_sprite = bg_is_white || sprite_in_front;

                        if use_sprite {
                            obj_pixel.color
                        } else {
                            bg_pixel.color
                        }
                    }
                } else {
                    bg_pixel.color
                }
            }
            _ => unreachable!(),
        }
    }

    fn pixeltransfer(&mut self, cycles: u8) {
        for _ in 0..cycles {
            self.fetcher.step(&mut self.bg_fifo, 2); // This is approximative and wrong (but okysh

            //There should be more than 8 pixels to pop the bg_fifo
            if self.bg_fifo.queue.len() < 9 {
                return; // do not pop yet
            }

            //If there are objects at current coordinates push their pixels into obj_fifo
            let current_x = self.popped_pixels as i32;
            if let (Some(obj)) = self.objects_at(current_x) {
                self.update_obj_fifo(obj);
            }

            //This both pops and mixes pixels
            let pixel_to_draw = self.mix_fifo_pixels();

            //We discard useless pixels, so we keep popping
            if self.popped_pixels >= self.fine_scroll_x as u16 {
                let visible_x = (self.popped_pixels - self.fine_scroll_x as u16) as usize;
                if visible_x < WIDTH {
                    let idx = self.read(LY) as usize * WIDTH + visible_x;
                    self.viewport[idx] = pixel_to_draw;
                }
            }
            self.popped_pixels += 1;
        }
    }

    fn hblank(&mut self, _cycles: u8) {
        //this does nothing
    }

    fn vblank(&mut self, cycles: u8) {
        self.fetcher.window_line = 0;
        self.vblank_line_clock += cycles as u16;

        while self.vblank_line_clock >= 456 {
            self.vblank_line_clock -= 456;
            self.increment_ly();
        }
    }

    // ============ Changing States ===========

    fn set_state(&mut self, state: State) {
        self.state = state.clone();

        let mut stat = self.read(STAT);
        stat = (stat & !0b11) | (state as u8 & 0b11); // update mode bits only
        self.write(STAT, stat);
    }

    fn change_to_state(&mut self, state: State, remaining_cycles: u8) {
        //This supposes we are always right when changing state, and
        //We are thus  changing to state with a correct timing
        self.clock = 0;
        self.set_state(state.clone());
        self.check_stat_interrupt();

        match state {
            HBlank => {
                self.hblank(remaining_cycles);
            }
            VBlank => {
                self.request_interrupt(INT_VBLANK);
                self.vblank(remaining_cycles);
            }
            PixelTransfer => {
                self.fetcher.tile_x = self.read(SCX) / 8;
                self.fine_scroll_x = self.read(SCX) % 8;
                self.popped_pixels = 0;

                self.bg_fifo.clear();
                self.obj_fifo.clear();
                self.fetcher.state = GetTileIndex;
                self.pixeltransfer(remaining_cycles);
            }
            OAMSearch => {
                self.clock = 0;
                self.line_objs = None;
                self.oamsearch(remaining_cycles);
            }
            Idle => todo!(),
        }
    }

    fn request_interrupt(&mut self, bit: u8) {
        let if_val = self.read(IF);
        self.write(IF, if_val | (1 << bit));
    }

    fn check_stat_interrupt(&mut self) {
        let stat = StatRegister::new(self.read(STAT));

        let interrupt_enabled = match self.state {
            HBlank => stat.mode0,
            VBlank => stat.mode1,
            OAMSearch => stat.mode2,
            _ => false,
        };

        if interrupt_enabled {
            self.request_interrupt(INT_STAT);
        }
    }

    fn increment_ly(&mut self) {
        let current_ly = self.read(LY);
        let new_ly = if current_ly >= 153 { 0 } else { current_ly + 1 };
        self.write(LY, new_ly);

        let lcdc = self.fetch_lcdc_register();
        let wy = self.read(WY);

        if lcdc.window_enabled && current_ly + 1 >= wy {
            // We are entering or already in window vertically
            // Increment window_line when LY *increases* into or past WY
            if current_ly < wy && new_ly >= wy {
                // First line of window: window_line becomes 0
                self.fetcher.window_line = 0;
            //TODO: Make this cleaner
            } else if new_ly >= wy && self.read(WX) < 167 && self.read(WX) > 6 {
                // Already in window: increment
                self.fetcher.window_line += 1;
            }
        } else {
            // Not in window vertically → reset
            self.fetcher.window_line = 0;
        }

        // LYC == LY compare flag (bit 2 of STAT)
        let lyc = self.read(LYC);
        let lyc_eq_ly = lyc == new_ly;

        let mut stat = self.read(STAT);
        if lyc_eq_ly {
            stat |= 0x04; // Set bit 2
            // If LYC=LY interrupt is enabled (STAT bit 6), request STAT interrupt
            if stat & (1 << 6) != 0 {
                self.request_interrupt(INT_STAT);
            }
        } else {
            stat &= !0x04; // Clear bit 2
        }
        self.write(STAT, stat);

        // Frame ready exactly when LY wraps from 153 → 0
        if current_ly == 153 && new_ly == 0 {
            self.framebuffer = Some(self.viewport.clone());
        }
    }

    pub fn is_frame_ready(&self) -> bool {
        matches!(self.framebuffer, Some(_))
    }

    pub fn clear_buffer(&mut self) {
        self.framebuffer = None;
    }

    pub fn yield_frame(&self) -> [u8; 23040] {
        self.framebuffer.clone().unwrap()
    }

    // =========== Running the PPU ==============

    fn state_duration(&self) -> u16 {
        match self.state {
            OAMSearch => 80,
            PixelTransfer => {
                let objs = self.line_objs.as_ref().unwrap().len();
                172 + objs as u16 * 12
            }
            HBlank => {
                let objs = self.line_objs.as_ref().unwrap().len();
                204 - objs as u16 * 12
            }
            VBlank => 4560,
            _ => unreachable!(),
        }
    }

    pub fn step(&mut self, cycles: u8) {
        let mut overflow = None;

        // Helper to consume cycles and detect overflow
        let mut consume = |duration: u16| -> u8 {
            let total = self.clock + cycles as u16;
            if total > duration {
                let remainder = (total - duration) as u8;
                overflow = Some(remainder);
                remainder
            } else {
                0
            }
        };

        /*
        );*/
        let consumed = match self.state {
            OAMSearch => {
                let remaining = consume(self.state_duration());
                self.oamsearch(cycles.saturating_sub(remaining));
                if remaining > 0 {
                    self.change_to_state(PixelTransfer, remaining);
                }
                cycles - remaining
            }

            PixelTransfer => {
                let remaining = consume(self.state_duration());
                self.pixeltransfer(cycles.saturating_sub(remaining));
                if remaining > 0 && self.popped_pixels >= (self.fine_scroll_x as u16 + 160) {
                    self.change_to_state(HBlank, remaining);
                }
                cycles - remaining
            }

            HBlank => {
                let remaining = consume(self.state_duration());
                self.hblank(cycles);
                if remaining > 0 {
                    let prev_ly = self.read(LY);
                    self.increment_ly();
                    let next_state = if prev_ly == 143 { VBlank } else { OAMSearch };
                    self.change_to_state(next_state, remaining);
                }
                cycles - remaining
            }

            VBlank => {
                let remaining = consume(self.state_duration());
                self.vblank(cycles.saturating_sub(remaining));
                if remaining > 0 {
                    self.framebuffer = Some(self.viewport.clone());
                    self.change_to_state(OAMSearch, remaining);
                }
                cycles - remaining
            }

            _ => todo!(),
        };

        // Update clock: if no overflow, add consumed cycles; otherwise, set to overflow
        self.clock = overflow
            .map(|o| o as u16)
            .unwrap_or_else(|| self.clock + consumed as u16);
    }
}

// ============= Pixel FIFO ============

#[derive(Copy, Clone)]
pub struct Pixel {
    pub color: u8, // 0–3 after palette
    pub bg_priority: bool,
    pub sprite_priority: bool,
    pub palette: Option<u8>,
}

struct PixelFIFO {
    queue: VecDeque<Pixel>,
}

use std::fmt;

impl fmt::Debug for PixelFIFO {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PixelFIFO(len={}) [", self.queue.len())?;

        for (i, px) in self.queue.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", px.color)?;
        }

        write!(f, "]")
    }
}

impl PixelFIFO {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::with_capacity(16),
        }
    }

    pub fn push(&mut self, px: Pixel) {
        if self.queue.len() < 16 {
            self.queue.push_back(px);
        }
    }

    pub fn pop(&mut self) -> Option<Pixel> {
        self.queue.pop_front()
    }

    pub fn can_push(&self) -> bool {
        //Seems like making this <= 8 and pop <9 made everything slower?
        self.queue.len() <= 8
    }

    pub fn clear(&mut self) {
        self.queue.clear();
    }

    pub fn is_empty(&self) -> bool {
        self.queue.len() == 0
    }

    pub fn push_tile_from_bytes(&mut self, low: u8, high: u8, obj: Option<Obj>) {
        let sprite_priority = obj.as_ref().map(|o| o.priority == 0).unwrap_or(false);
        let palette = obj.as_ref().map(|o| Some(o.palette)).unwrap_or(None);

        for bit in (0..8).rev() {
            let low_bit = (low >> bit) & 1;
            let high_bit = (high >> bit) & 1;
            let color = (high_bit << 1) | low_bit;

            self.push(Pixel {
                color,
                bg_priority: false,
                sprite_priority,
                palette,
            });
        }
    }
}

// ============= Pixel Fetcher ============
#[derive(Copy, Clone, Debug)]
// The first 4 steps take 2 dots each and the fifth step is attempted every dot until it succeeds
enum FetcherState {
    GetTileIndex,
    GetTileLow,
    GetTileHigh,
    Sleep,
    PushToFifo,
}

struct PixelFetcher {
    bus: Rc<RefCell<Bus>>,
    clock: u8,
    state: FetcherState,
    internal_ly: u8,
    tile_x: u8, // Current horizontal tile index
    tile_y: u8, // Current vertical tile index (or LY / 8)
    tile_index: u8,
    low_byte: u8,
    high_byte: u8,

    window_line: u8, // NEW: track how many lines of the window have been drawn
    ly_where_window_is_active: u8,

    discard_pixels: u8,
}

impl fmt::Debug for PixelFetcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "PixelFetcher(state={:?}, tile=({},{}), index={}, low=0x{:02X}, high=0x{:02X}, clock={})",
            self.state,
            self.tile_x,
            self.tile_y,
            self.tile_index,
            self.low_byte,
            self.high_byte,
            self.clock
        )
    }
}

use FetcherState::*;
impl PixelFetcher {
    fn new(bus: Rc<RefCell<Bus>>) -> Self {
        Self {
            bus: bus,
            state: FetcherState::GetTileIndex,

            internal_ly: 0,
            tile_x: 0,
            tile_y: 0,

            tile_index: 0,
            low_byte: 0,
            high_byte: 0,
            clock: 0,

            window_line: 0,
            ly_where_window_is_active: 0,
            discard_pixels: 0,
        }
    }

    //TODO! This is lazy
    fn fetch_lcdc_register(&self) -> LcdcRegister {
        LcdcRegister::new(self.read(LCDC))
    }

    fn fetch_tile_bytes_unsigned(&self, index: u8) -> TileBytes {
        let mut bytes: [u8; 16] = [0x00; 16];

        // Tile address for unsigned mode (0x8000 base)
        let address: u16 = TILE_DATA_BASE_UNSIGNED
            .checked_add((index as u16) * 16)
            .expect("Unsigned tile address overflow");

        for i in 0..16 {
            bytes[i as usize] = self.read(address + i as u16);
        }

        bytes
    }

    fn fetch_tile_bytes_signed(&self, index: i8) -> TileBytes {
        let mut bytes: [u8; 16] = [0x00; 16];

        // Tile address for signed mode (0x9000 base)
        // Convert to i32 to safely handle negaive indices
        let address: u16 = (TILE_DATA_BASE_SIGNED as i32 + (index as i32 * 16)) as u16;

        for i in 0..16 {
            bytes[i as usize] = self.read(address + i as u16);
        }
        bytes
    }

    /*
    fn using_window(&self, lcdc: &LcdcRegister, wx: u8, wy: u8, scx: u8) -> bool {
        let pixel_x = (self.tile_x as u16) * 8;
        let visible_x = (pixel_x as i16 - scx as i16).clamp(0, 159) as u8;
        lcdc.window_enabled && self.internal_ly >= wy && visible_x >= wx
    }*/
    fn using_window(&self, lcdc: &LcdcRegister, wx: u8, wy: u8) -> bool {
        if !lcdc.window_enabled {
            return false;
        }
        if self.internal_ly < wy {
            return false;
        }
        // Current pixel X position on screen (not affected by SCX!)
        let current_pixel_x = (self.tile_x as u16) * 8; // 0, 8, 16, ...

        current_pixel_x >= wx as u16
    }

    fn get_tile_idx(&mut self) {
        let lcdc = self.fetch_lcdc_register();
        self.internal_ly = self.read(LY);
        let scx = self.read(SCX);
        let scy = self.read(SCY);
        let wx = self.read(WX).wrapping_sub(7) as u8; // Window X is offset by 7
        let wy = self.read(WY);

        let using_window = self.using_window(&lcdc, wx, wy); //, scx);

        // Tilemap address
        let tilemap_address = if using_window {
            if lcdc.window_tilemap {
                TILE_MAP1_ADDRESS
            } else {
                TILE_MAP0_ADDRESS
            }
        } else {
            if lcdc.bg_tilemap {
                TILE_MAP1_ADDRESS
            } else {
                TILE_MAP0_ADDRESS
            }
        };

        let tile_x = if using_window {
            let screen_pixel_x = (self.tile_x as u16) * 8;
            // Safe saturating sub on u16 then /8 back to tile index
            ((screen_pixel_x.saturating_sub(wx as u16) / 8) & 0x1F) as u8
        } else {
            // For background: fetcher.tile_x already holds SCX/8 at start and increments
            // every tile pushed. Use it directly (wrap to 0..31).
            (self.tile_x & 0x1F) as u8
        };

        let tile_y = if using_window {
            self.window_line / 8
        } else {
            (((self.internal_ly as u16 + scy as u16) >> 3) & 0xFF) as u8
        };

        let byte_address = tilemap_address + (tile_y as u16) * 32 + (tile_x as u16);

        // VRAM access check (mode 3 blocks VRAM) TODO!
        let tile_index = self.read(byte_address);

        self.tile_index = tile_index;
        self.state = FetcherState::GetTileLow;
    }

    //These two are basically rewriting what we did before in the fetch_current_tilemap, and tile
    //functions, but are actually useful, for other than understanding
    fn get_tile_low(&mut self) {
        let lcdc = self.fetch_lcdc_register();
        let scy = self.read(SCY);
        let wx = self.read(WX).wrapping_sub(7);
        let wy = self.read(WY);

        // Determine if this tile is part of the window
        let using_window = self.using_window(&lcdc, wx, wy); //, 0);

        //Vertical pixel within the tile (0..7)
        let fine_y = if using_window {
            ((self.window_line) % 8) as u8
        } else {
            ((self.internal_ly as u16 + scy as u16) % 8) as u8
        };

        // Determine addressing mode
        let signed_addressing = !lcdc.bg_window_tiles; // LCDC.4
        let tile_bytes = if signed_addressing {
            self.fetch_tile_bytes_signed(self.tile_index as i8)
        } else {
            self.fetch_tile_bytes_unsigned(self.tile_index)
        };

        self.low_byte = tile_bytes[(fine_y as usize) * 2]; // low byte of row
        self.state = FetcherState::GetTileHigh;
    }
    fn get_tile_high(&mut self) {
        let lcdc = self.fetch_lcdc_register();
        let scy = self.read(SCY);
        let wx = self.read(WX).wrapping_sub(7);
        let wy = self.read(WY);

        // Determine if this tile is part of the window
        let using_window = self.using_window(&lcdc, wx, wy); //, 0);

        //Is it ok to even use ly, wy, scy, here?
        let fine_y = if using_window {
            ((self.window_line) % 8) as u8
        } else {
            ((self.internal_ly as u16 + scy as u16) % 8) as u8
        };

        let signed_addressing = !lcdc.bg_window_tiles;

        let tile_bytes = if signed_addressing {
            self.fetch_tile_bytes_signed(self.tile_index as i8)
        } else {
            self.fetch_tile_bytes_unsigned(self.tile_index)
        };

        self.high_byte = tile_bytes[(fine_y as usize) * 2 + 1]; // high byte of row
        self.state = FetcherState::PushToFifo;
    }

    //Q1: How many pixels does this push?
    fn push_to_fifo(&mut self, fifo: &mut PixelFIFO) {
        // Push 8 pixels from low/high bytes to FIFO
        if !fifo.can_push() {
            return; //Equivalent to sleeping!
        }

        fifo.push_tile_from_bytes(self.low_byte, self.high_byte, None);

        self.tile_x = self.tile_x.wrapping_add(1);
        self.state = FetcherState::GetTileIndex;
    }

    fn step(&mut self, fifo: &mut PixelFIFO, mut cycles: u8) {
        match self.state {
            GetTileIndex => {
                self.get_tile_idx();
                cycles = cycles - 2;
                if cycles == 0 {
                    return;
                }
            }
            GetTileLow => {
                self.get_tile_low();
                cycles = cycles - 2;
                if cycles == 0 {
                    return;
                }
            }
            GetTileHigh => {
                self.get_tile_high();
                cycles = cycles - 2;
                if cycles == 0 {
                    return;
                }
            }
            PushToFifo => {
                self.push_to_fifo(fifo);
                cycles = cycles - 2;
                if cycles == 0 {
                    return;
                }
            }
            Sleep => {}
        }
    }
}
