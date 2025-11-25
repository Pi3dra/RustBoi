#![allow(dead_code, unused_variables)]

use crate::bus::Bus;
use crate::bus::BusAccess;
use std::cell::RefCell;
use std::rc::Rc;

//Try this: https://robertheaton.com/gameboy-doctor/

const ZERO: u8 = 7; //Z
const SUBSTRACTION: u8 = 6; //N
const HALFCARRY: u8 = 5; //H
const CARRY: u8 = 4; //C

//Some Timer Registers:

const DIV: u16 = 0xFF04;
const TIMA: u16 = 0xFF05;
const TMA: u16 = 0xFF06;
const TAC: u16 = 0xFF07;

//Interrupt registers
const IF: u16 = 0xFF0F; //Interrupt flag
const IE: u16 = 0xFFFF; //Interrupt enable

use super::{FlagCondition, MemAdress, Operand, Reg8, Reg16};
use crate::cpu::opcodes::InstrPointer;
use std::fmt;

// ================================== CPU =============================

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
enum Interrupt {
    VBlank = 0x40,
    LCDStat = 0x48,
    Timer = 0x50, // Current bug <- Jumping here without asking, weird
    Serial = 0x58,
    Joypad = 0x60,
}

// Making the opcode decoding a child of CPU!
pub struct CPU {
    bus: Rc<RefCell<Bus>>,
    registers: Registers,
    clock: u64,
    ime: bool,         // Interrupt Master Enable
    ime_pending: bool, // For delayed EI
    halted: bool,
    opcode_table: [InstrPointer; 256],
    cb_table: [InstrPointer; 256],
    //for debug purposes
    executing: (u8, InstrPointer),

    //Timer
    div_counter: u16,
}

use FlagCondition::*;
use MemAdress::*;
use Operand::*;
use Reg16::*;

impl BusAccess for CPU {
    fn read(&self, addr: u16) -> u8 {
        self.bus.borrow_mut().read(addr, true)
    }

    fn write(&mut self, addr: u16, value: u8) {
        self.bus.borrow_mut().write(addr, value, true)
    }
}

impl CPU {
    pub fn print_state(&mut self) {
        let pc_mem = [
            self.read(self.registers.pc),
            self.read(self.registers.pc.wrapping_add(1)),
            self.read(self.registers.pc.wrapping_add(2)),
            self.read(self.registers.pc.wrapping_add(3)),
        ];

        println!(
            "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X} DIV:{:02X} TIMA:{:02X} TMA:{:02X} TAC:{:02X} IE:{:02X} IF:{:02X}",
            self.registers.a,
            self.registers.f,
            self.registers.b,
            self.registers.c,
            self.registers.d,
            self.registers.e,
            self.registers.h,
            self.registers.l,
            self.registers.sp,
            self.registers.pc,
            pc_mem[0],
            pc_mem[1],
            pc_mem[2],
            pc_mem[3],
            self.read(DIV),
            self.read(TIMA),
            self.read(TMA),
            self.read(TAC),
            self.read(IE),
            self.read(IF),
        );
    }

    pub fn new(bus: Rc<RefCell<Bus>>) -> Self {
        let (opcode_table, cb_table) = CPU::build_table();

        //println! {"{:?}",opcode_table[0xFA]};

        //Blarg tests supposes that we start after BIOS exec
        let registers = Registers {
            a: 0x01,
            f: 0xB0,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            h: 0x01,
            l: 0x4D,
            sp: 0xFFFE,
            pc: 0x0100,
        };

        let executing = (0, InstrPointer::None);

        let mut cpu = CPU {
            registers,
            bus,
            clock: 0,
            ime: false,
            ime_pending: false,
            halted: false,
            opcode_table,
            cb_table,
            executing,
            div_counter: 0,
        };

        cpu.write(IE, 0x10);
        cpu
    }
    pub fn step(&mut self) -> u8 {
        self.update_ime();
        if self.halted {
            self.clock = self.clock.wrapping_add(4);
            self.advance_timer(4);
            if self.interrupt_pending() {
                self.halted = false;
                if self.ime {
                    self.handle_interrupts();
                } else {
                    self.registers.pc = self.registers.pc.wrapping_add(1); // Skip 1st instr after HALT 
                }
            }
            return 4;
        }
        //CPU::print_state(self);

        let pc = self.registers.get_16register(PC);
        let opcode = self.read(pc);
        let cycles: u8 = {
            if opcode == 0xCB {
                let opcode2 = self.read(self.registers.pc.wrapping_add(1));
                self.registers.pc = pc.wrapping_add(2);
                self.execute_from_instr(self.cb_table[opcode2 as usize], opcode2)
            } else {
                self.registers.pc = pc.wrapping_add(1);
                self.execute_from_instr(self.opcode_table[opcode as usize], opcode)
            }
        };
        self.handle_interrupts();
        cycles
    }

    pub fn run(&mut self) {
        loop {
            self.step();
            /* Optional: safety cutoff to prevent infinite loops
            if self.clock > 50_000_000 {
                println!("❌ Timeout or infinite loop. Test failed or hanging.");
                break;
            }*/
        }
    }

    fn execute_from_instr(&mut self, instr: InstrPointer, opcode: u8) -> u8 {
        self.executing = (opcode, instr);
        let clock1 = self.clock;
        let cycles = match instr {
            InstrPointer::Const(func, c) => {
                func(self);
                c
            }
            InstrPointer::Unop(func, op, c) => {
                func(self, op);
                c
            }
            InstrPointer::Binop(func, op1, op2, c) => {
                func(self, op1, op2);
                c
            }
            InstrPointer::None => panic!("Unimplemented opcode"),
        };

        // Add cycles to clock
        self.clock = self.clock.wrapping_add(cycles as u64);

        let true_cycles = self.clock - clock1;
        // Update timer //TODO make all clock additions u8, no need for u16!
        self.advance_timer(true_cycles as u8);

        true_cycles as u8
    }

    fn check_condition(&mut self, op: Operand) -> bool {
        match op {
            Flag(NZ) => return !self.registers.get_flag(ZERO),
            Flag(Z) => return self.registers.get_flag(ZERO),
            Flag(NC) => return !self.registers.get_flag(CARRY),
            Flag(C) => return self.registers.get_flag(CARRY),
            Flag(None) => true,
            _ => panic!("not a flag condition!"),
        }
    }

    fn get_operand_as_u8(&mut self, op: Operand) -> u8 {
        match op {
            R8(register) => self.registers.get_u8register(register),

            Address(AddrR16(register)) => {
                let address = self.registers.get_16register(register);
                self.read(address)
            }
            Address(HLInc) => {
                let address = self.registers.get_16register(HL);
                self.registers.set_16register(HL, address.wrapping_add(1));
                return self.read(address);
            }
            Address(HLDec) => {
                let address = self.registers.get_16register(HL);
                self.registers.set_16register(HL, address.wrapping_sub(1));
                return self.read(address);
            }
            Imm8 => {
                let address = self.registers.get_16register(PC);
                self.registers.set_16register(PC, address.wrapping_add(1));
                return self.read(address);
            }
            Address(ImmAddr16) => {
                let lsb = self.get_operand_as_u8(Imm8);
                let msb = self.get_operand_as_u8(Imm8);
                let address = CPU::fuse_u8(lsb, msb);
                //println!("{:02X} at {:02X}", self.bus.read(address), address);
                return self.read(address);
            }

            //For LDH func
            Address(ImmAddr8) => {
                let offset = self.get_operand_as_u8(Imm8);
                let address = CPU::fuse_u8(offset, 0xFF);
                return self.read(address);
            }
            Address(AddrR8(register)) => {
                let offset = self.registers.get_u8register(register);
                let address = 0xFF00u16 + offset as u16;
                return self.read(address);
            }
            Value(n) => n as u8,

            //Address(Imm8) -> Do thhis
            _ => panic!("not a u8 operand for get!"),
        }
    }

    fn set_operand_from_u8(&mut self, op: Operand, value: u8) {
        match op {
            R8(register) => self.registers.set_u8register(register, value),

            Address(AddrR16(register)) => {
                let address = self.registers.get_16register(register);
                self.memwrite(address, value)
            }
            Address(HLInc) => {
                let address = self.registers.get_16register(HL);
                self.memwrite(address, value);
                self.registers.set_16register(HL, address.wrapping_add(1));
            }
            Address(HLDec) => {
                let address = self.registers.get_16register(HL);
                self.memwrite(address, value);
                self.registers.set_16register(HL, address.wrapping_sub(1));
            }
            Address(ImmAddr16) => {
                let lsb = self.get_operand_as_u8(Imm8);
                let msb = self.get_operand_as_u8(Imm8);
                let address = CPU::fuse_u8(lsb, msb);
                self.memwrite(address, value);
            }
            //For LDH func
            Address(ImmAddr8) => {
                let offset = self.get_operand_as_u8(Imm8);
                let address = 0xFF00u16 + offset as u16;
                self.memwrite(address, value)
            }
            Address(AddrR8(register)) => {
                let offset = self.registers.get_u8register(register);
                let address = 0xFF00u16 + offset as u16;
                self.memwrite(address, value)
            }

            _ => panic!("not a u8 operand for set!"),
        }
    }

    fn set_operand_to_u16(&mut self, op: Operand, value: u16) {
        match op {
            R16(register) => return self.registers.set_16register(register, value),
            Address(ImmAddr16) => {
                let lsb = self.get_operand_as_u8(Imm8);
                let msb = self.get_operand_as_u8(Imm8);

                let address = CPU::fuse_u8(lsb, msb);
                let (vlsb, vmsb) = CPU::split_u16(value);
                self.memwrite(address, vlsb);
                self.memwrite(address + 1, vmsb);
            }

            _ => panic!("not a u16 operand for set"),
        }
    }

    fn get_operand_as_u16(&mut self, op: Operand) -> u16 {
        match op {
            R16(register) => return self.registers.get_16register(register),
            Imm16 => {
                let lsb: u8 = self.get_operand_as_u8(Imm8);
                let msb: u8 = self.get_operand_as_u8(Imm8);
                return CPU::fuse_u8(lsb, msb);
            }
            Address(Fixed(value)) => return value,
            Value(value) => return value,
            _ => panic!("not a u16 operand for get! "),
        }
    }

    fn update_flags(&mut self, zero: bool, sub: bool, halfcarry: bool, carry: bool) {
        self.registers.set_flag(ZERO, zero);
        self.registers.set_flag(SUBSTRACTION, sub);
        self.registers.set_flag(HALFCARRY, halfcarry);
        self.registers.set_flag(CARRY, carry);
    }

    fn split_u16(value: u16) -> (u8, u8) {
        let lsb = (value & 0xFF) as u8;
        let msb = (value >> 8) as u8;
        (lsb, msb)
    }

    fn fuse_u8(lsb: u8, msb: u8) -> u16 {
        ((msb as u16) << 8) | (lsb as u16)
    }

    fn memwrite(&mut self, address: u16, value: u8) {
        if address == DIV {
            self.div_counter = 0;
            self.write(DIV, 0);
            return;
        }

        self.write(address, value);
    }
    // ============= Loading =============

    //this implements both ldh and ld
    pub(crate) fn ld_u8(&mut self, destination: Operand, source: Operand) {
        let val_to_load: u8 = self.get_operand_as_u8(source);
        self.set_operand_from_u8(destination, val_to_load);
    }

    pub(crate) fn ld_u16(&mut self, destination: Operand, source: Operand) {
        // I think this doesn't properly implement LD [a16] SP
        let val_to_load: u16 = self.get_operand_as_u16(source);
        self.set_operand_to_u16(destination, val_to_load);
    }

    pub(crate) fn ld_u16_e8(&mut self, destination: Operand, _source: Operand) {
        let e8 = self.get_operand_as_u8(Operand::Imm8) as i8;
        let sp = self.get_operand_as_u16(Operand::R16(Reg16::SP));

        let result = sp.wrapping_add_signed(e8 as i16);

        self.set_operand_to_u16(destination, result);

        let h = ((sp & 0xF) + ((e8 as u16) & 0xF)) > 0xF;
        let c = ((sp & 0xFF) + ((e8 as u16) & 0xFF)) > 0xFF;
        self.update_flags(false, false, h, c);
    }

    // ============= Arithmetic =============

    pub(crate) fn add(&mut self, op1: Operand, op2: Operand) {
        let value1: u8 = self.get_operand_as_u8(op1);
        let value2: u8 = self.get_operand_as_u8(op2);
        let (result, overflowed) = value1.overflowing_add(value2);

        //Addition always stores back on a register
        self.set_operand_from_u8(op1, result);

        let half_carry: bool = ((value1 & 0xF) + (value2 & 0xF)) > 0xF;
        self.update_flags(result == 0, false, half_carry, overflowed);
    }

    pub(crate) fn adc(&mut self, op1: Operand, op2: Operand) {
        let a: u8 = self.get_operand_as_u8(op1);
        let n: u8 = self.get_operand_as_u8(op2);
        let carry_in: u8 = self.registers.get_flag(CARRY) as u8;

        let sum16 = a as u16 + n as u16 + carry_in as u16;
        let result = sum16 as u8;
        let half_carry = ((a & 0xF) + (n & 0xF) + carry_in) > 0xF;
        let carry = sum16 > 0xFF;

        self.set_operand_from_u8(op1, result);
        self.update_flags(result == 0, false, half_carry, carry);
    }

    pub(crate) fn sbc(&mut self, op1: Operand, op2: Operand) {
        let a = self.get_operand_as_u8(op1);
        let n = self.get_operand_as_u8(op2);
        let carry_in = self.registers.get_flag(CARRY) as u8;

        let n_and_carry = n.wrapping_add(carry_in);

        let result = a.wrapping_sub(n_and_carry);

        let half_carry = (a & 0xF) < ((n & 0xF) + carry_in);
        let carry = (a as u16) < (n as u16 + carry_in as u16);

        self.set_operand_from_u8(op1, result);
        self.update_flags(result == 0, true, half_carry, carry);
    }

    pub(crate) fn inc(&mut self, op1: Operand) {
        let value: u8 = self.get_operand_as_u8(op1);
        let (result, overflowed) = value.overflowing_add(1);

        //Addition always stores back on a register
        self.set_operand_from_u8(op1, result);

        let half_carry: bool = ((value & 0xF) + (1 & 0xF)) > 0xF;
        let carry: bool = self.registers.get_flag(CARRY);
        self.update_flags(result == 0, false, half_carry, carry);
    }

    pub(crate) fn sub(&mut self, op1: Operand, op2: Operand) {
        let value1: u8 = self.get_operand_as_u8(op1);
        let value2: u8 = self.get_operand_as_u8(op2);
        let result = value1.wrapping_sub(value2);

        //Addition always stores back on a register
        self.set_operand_from_u8(op1, result);

        let half_carry: bool = (value1 & 0xF) < (value2 & 0xF);
        let c_flag = value1 < value2; // full-borrow
        self.update_flags(result == 0, true, half_carry, c_flag);
    }

    pub(crate) fn dec(&mut self, op1: Operand) {
        let value: u8 = self.get_operand_as_u8(op1);
        let result = value.wrapping_sub(1);

        self.set_operand_from_u8(op1, result);

        let half_carry: bool = (value & 0xF) == 0;
        let keep_carry: bool = self.registers.get_flag(CARRY);
        self.update_flags(result == 0, true, half_carry, keep_carry);
    }

    pub(crate) fn and(&mut self, source: Operand, op: Operand) {
        let register_value = self.get_operand_as_u8(source);
        let result: u8 = register_value & self.get_operand_as_u8(op);

        self.set_operand_from_u8(source, result);
        self.update_flags(result == 0, false, true, false);
    }

    pub(crate) fn or(&mut self, source: Operand, op: Operand) {
        let register_value = self.get_operand_as_u8(source);
        let result: u8 = register_value | self.get_operand_as_u8(op);

        self.set_operand_from_u8(source, result);
        self.update_flags(result == 0, false, false, false);
    }

    pub(crate) fn xor(&mut self, source: Operand, op: Operand) {
        let register_value = self.get_operand_as_u8(source);
        let result: u8 = register_value ^ self.get_operand_as_u8(op);
        self.set_operand_from_u8(source, result);

        self.update_flags(result == 0, false, false, false);
    }

    pub(crate) fn cp(&mut self, source: Operand, op: Operand) {
        let register_value = self.get_operand_as_u8(source);
        let to_sub: u8 = self.get_operand_as_u8(op);

        let (result, carry) = register_value.overflowing_sub(to_sub);

        let half_carry: bool = (register_value & 0xF) < (to_sub & 0xF);
        self.update_flags(result == 0, true, half_carry, carry);
    }

    // ============= reg16 Arithmetic =============
    pub(crate) fn inc_u16(&mut self, op: Operand) {
        let value = self.get_operand_as_u16(op);
        self.set_operand_to_u16(op, value.wrapping_add(1));
    }

    pub(crate) fn dec_u16(&mut self, op: Operand) {
        let value = self.get_operand_as_u16(op);
        self.set_operand_to_u16(op, value.wrapping_sub(1));
    }

    pub(crate) fn add_hl_rr(&mut self, src: Operand) {
        let hl = self.registers.get_16register(Reg16::HL);
        let value = self.get_operand_as_u16(src);

        let (result, carry) = hl.overflowing_add(value);
        let half_carry = ((hl & 0x0FFF) + (value & 0x0FFF)) > 0x0FFF;

        self.registers.set_16register(Reg16::HL, result);
        self.update_flags(self.registers.get_flag(ZERO), false, half_carry, carry);
    }

    pub(crate) fn add_sp_e8(&mut self) {
        let sp = self.registers.get_16register(Reg16::SP);
        let offset = self.get_operand_as_u8(Operand::Imm8) as i8 as i16; // signed immediate

        let result = sp.wrapping_add(offset as u16);

        let half_carry = ((sp & 0xF) + ((offset as u16) & 0xF)) > 0xF;
        let carry = ((sp & 0xFF) + ((offset as u16) & 0xFF)) > 0xFF;

        self.registers.set_16register(Reg16::SP, result);
        self.update_flags(false, false, half_carry, carry);
    }

    // ============= Jumps and Calls =============

    pub(crate) fn jr(&mut self, condition: Operand, op: Operand) {
        // this increments PC
        let offset = self.get_operand_as_u8(op) as i8 as i16;

        if self.check_condition(condition) {
            let pc: u16 = self.registers.get_16register(PC);
            self.registers
                .set_16register(PC, (pc as i16).wrapping_add(offset) as u16);
            self.clock = self.clock.wrapping_add(12 as u64);
        } else {
            self.clock = self.clock.wrapping_add(8 as u64);
        }
    }

    pub(crate) fn jp(&mut self, condition: Operand, address: Operand) {
        let address = self.get_operand_as_u16(address);
        if self.check_condition(condition) {
            self.registers.set_16register(PC, address);
            if matches!(R16(HL), address) {
                self.clock = self.clock.wrapping_add(4);
            } else {
                self.clock = self.clock.wrapping_add(16);
            }
        } else {
            self.clock = self.clock.wrapping_add(12 as u64);
        }
    }

    pub(crate) fn call(&mut self, condition: Operand, address_operand: Operand) {
        let addr = self.get_operand_as_u16(address_operand);
        if self.check_condition(condition) {
            let pc = self.registers.get_16register(PC);
            let mut sp = self.registers.get_16register(SP);

            //Address to jump to

            //storing PC in stack
            let (lsb, msb) = CPU::split_u16(pc);

            sp = sp.wrapping_sub(1);
            self.memwrite(sp, msb); // LSB
            sp = sp.wrapping_sub(1);
            self.memwrite(sp, lsb); // MSB

            //updating registers accordingly
            self.registers.set_16register(SP, sp);
            self.registers.set_16register(PC, addr);

            self.clock = self.clock.wrapping_add(24 as u64);
        } else {
            self.clock = self.clock.wrapping_add(12 as u64);
        }
    }

    pub(crate) fn rst(&mut self, address: Operand) {
        let addr = self.get_operand_as_u16(address);
        let pc = self.registers.get_16register(PC);
        let mut sp = self.registers.get_16register(SP);

        // Push PC to stack in little-endian order: MSB first, then LSB
        let msb = (pc >> 8) as u8;
        let lsb = (pc & 0xFF) as u8;

        sp = sp.wrapping_sub(1);
        self.memwrite(sp, msb);
        sp = sp.wrapping_sub(1);
        self.memwrite(sp, lsb);

        self.registers.set_16register(SP, sp);
        self.registers.set_16register(PC, addr);

        self.clock += 16; // RST always 16 cycles
    }

    pub(crate) fn ret(&mut self, condition: Operand) {
        if self.check_condition(condition) {
            let mut sp = self.registers.get_16register(SP);
            //Not sure of ordering here, is the stack little or big endian?
            let lsb = self.read(sp);
            sp = sp.wrapping_add(1);
            let msb = self.read(sp);
            sp = sp.wrapping_add(1);

            let address = CPU::fuse_u8(lsb, msb);
            self.registers.set_16register(SP, sp);
            self.registers.set_16register(PC, address);

            if matches!(Operand::Flag(None), condition) {
                self.clock = self.clock.wrapping_add(16 as u64);
                self.clock += 16;
            } else {
                self.clock = self.clock.wrapping_add(20 as u64);
            }
        } else {
            self.clock = self.clock.wrapping_add(8 as u64);
        }
    }

    pub(crate) fn reti(&mut self) {
        self.ret(Flag(None));
        self.ei();
    }

    pub(crate) fn push(&mut self, op: Operand) {
        let value = self.get_operand_as_u16(op);
        let (lsb, msb) = CPU::split_u16(value);

        let mut sp = self.registers.get_16register(SP);

        sp = sp.wrapping_sub(1);
        self.memwrite(sp, msb);
        sp = sp.wrapping_sub(1);
        self.memwrite(sp, lsb);

        self.registers.set_16register(SP, sp);
    }

    pub(crate) fn pop(&mut self, op: Operand) {
        let mut sp = self.registers.get_16register(SP);

        let lsb = self.read(sp);
        sp = sp.wrapping_add(1);
        let msb = self.read(sp);
        sp = sp.wrapping_add(1);

        let value = CPU::fuse_u8(lsb, msb);

        self.set_operand_to_u16(op, value);
        self.registers.set_16register(SP, sp);
    }

    // ============= Interrupts and Misc =============

    pub(crate) fn ei(&mut self) {
        self.ime_pending = true;
    }

    pub(crate) fn di(&mut self) {
        self.ime = false; // Immediately disable interrupts
        self.ime_pending = false; // Make sure no pending enable remains
    }

    pub(crate) fn nop(&mut self) {}

    pub(crate) fn halt(&mut self) {
        self.halted = true;

        /*

        FOR FUTURE ME:
        You need to handle the HALT bug:
        if IME = 0 and an interrupt is pending,
        the PC increments incorrectly.
        while running {
            if cpu.halted {
            // Resume execution only if an interrupt is pending
            if cpu.interrupt_pending() {
                cpu.halted = false;
            } else {
                // CPU is halted, skip instruction fetch/execute
                cpu.clock += 4; // CPU still increments clock slowly
                continue;
            }
        }
        */
    }

    //set carry flag
    pub(crate) fn scf(&mut self) {
        let zero = self.registers.get_flag(ZERO);
        self.update_flags(zero, false, false, true);
    }

    //complement carry flag
    pub(crate) fn ccf(&mut self) {
        let zero = self.registers.get_flag(ZERO);
        let carry = self.registers.get_flag(CARRY);
        self.update_flags(zero, false, false, !carry);
    }

    pub(crate) fn cpl(&mut self) {
        let a = self.registers.get_u8register(Reg8::A);
        let result = !a;

        self.registers.set_u8register(Reg8::A, result);
        self.registers.set_flag(SUBSTRACTION, true);
        self.registers.set_flag(HALFCARRY, true);
    }

    pub(crate) fn daa(&mut self) {
        let mut a = self.registers.a;
        let mut adjust = 0u8;

        if !self.registers.get_flag(SUBSTRACTION) {
            if self.registers.get_flag(HALFCARRY) || (a & 0x0F) > 9 {
                adjust |= 0x06;
            }
            if self.registers.get_flag(CARRY) || a > 0x99 {
                adjust |= 0x60;
                self.registers.set_flag(CARRY, true);
            }
            a = a.wrapping_add(adjust);
        } else {
            if self.registers.get_flag(HALFCARRY) {
                adjust |= 0x06;
            }
            if self.registers.get_flag(CARRY) {
                adjust |= 0x60;
            }
            a = a.wrapping_sub(adjust);
        }

        self.registers.set_flag(ZERO, a == 0);
        self.registers.a = a;
        self.registers.set_flag(HALFCARRY, false);
    }

    pub(crate) fn rlca(&mut self) {
        self.rlc(R8(Reg8::A));
        self.clock = self.clock.wrapping_sub(4);
        let keep_carry = self.registers.get_flag(CARRY);
        self.update_flags(false, false, false, keep_carry);
    }
    pub(crate) fn rla(&mut self) {
        self.rl(R8(Reg8::A));
        self.clock = self.clock.wrapping_sub(4);
        let keep_carry = self.registers.get_flag(CARRY);
        self.update_flags(false, false, false, keep_carry);
    }
    pub(crate) fn rrca(&mut self) {
        self.rrc(R8(Reg8::A));
        self.clock = self.clock.wrapping_sub(4);
        let keep_carry = self.registers.get_flag(CARRY);
        self.update_flags(false, false, false, keep_carry);
    }
    pub(crate) fn rra(&mut self) {
        self.rr(R8(Reg8::A));
        self.clock = self.clock.wrapping_sub(4);
        let keep_carry = self.registers.get_flag(CARRY);
        self.update_flags(false, false, false, keep_carry);
    }

    pub(crate) fn stop(&mut self, op: Operand) {
        self.reset_div();
        //println!("STOP");
        //TODO: RESET DIV ON STOP
        //panic!("Is STOP really needed?");
        //This has some weird hardware behavior
        //Chek a lot of documentation if implemented in future!
    }
    // Call this after every instruction fetch/execute
    fn update_ime(&mut self) {
        if self.ime_pending {
            self.ime = true;
            self.ime_pending = false;
        }
    }

    fn interrupt_pending(&mut self) -> bool {
        let (ie, iflag) = self.get_interrupt_registers();
        (ie & iflag) != 0
    }

    fn get_interrupt_registers(&mut self) -> (u8, u8) {
        let ie = self.read(IE);
        let iflag = self.read(IF);
        (ie, iflag)
    }

    const PRIORITY: [Interrupt; 5] = [
        Interrupt::VBlank,
        Interrupt::LCDStat,
        Interrupt::Timer,
        Interrupt::Serial,
        Interrupt::Joypad,
    ];

    fn handle_interrupts(&mut self) {
        let (ie, iflag) = self.get_interrupt_registers();
        let pending = ie & iflag;

        if !self.ime {
            return;
        }

        for (idx, interrupt) in CPU::PRIORITY.iter().enumerate() {
            let mask = 1 << idx;
            let is_pending = pending & mask != 0;
            if is_pending {
                
                
                if matches!(interrupt, Interrupt::Timer) {
                    continue;
                }
                if matches!(interrupt, Interrupt::Joypad) {
                    println!(
                        "Servicing : {:?} IE : {} IF : {} Jumps to {:02X}",
                        interrupt,
                        self.read(IE),
                        self.read(IF),
                        *interrupt as u16
                    );
                }

                let new_iflag = iflag & !mask;
                self.write(IF, new_iflag);
                self.service_interrupt(*interrupt as u16);
                return;
            }
        }
    }

    fn service_interrupt(&mut self, vector: u16) {
        // Push current PC (points to next instruction)
        let pc = self.registers.pc;
        let (low, high) = (pc as u8, (pc >> 8) as u8);

        let mut sp = self.registers.sp;
        sp = sp.wrapping_sub(1);
        self.memwrite(sp, high);
        sp = sp.wrapping_sub(1);
        self.memwrite(sp, low);
        self.registers.sp = sp;

        self.registers.pc = vector;

        // Interrupt takes 20 cycles total (12 for push + 8 for jump)
        self.clock = self.clock.wrapping_add(20);

        // Disable interrupts
        self.ime = false;
        self.ime_pending = false;
    }

    //Timer
    fn reset_div(&mut self) {
        self.div_counter = 0;
        self.write(DIV, 0);
    }

    fn tick_div(&mut self) {
        self.div_counter = self.div_counter.wrapping_add(1);
        self.write(DIV, self.div_counter as u8);
    }

    fn tac_info(&mut self) -> (bool, u8) {
        let tac = self.read(TAC);
        let timer_enabled = tac & 0x04 != 0;
        let clock_select = tac & 0x03;
        let div_bit_to_count: u8 = {
            match clock_select {
                0x00 => 7,
                0x01 => 1,
                0x10 => 3,
                0x11 => 5,
                _ => 1, //unreachable!(), HOW IS THIS EVEN POSSIBLE?
            }
        };
        (timer_enabled, div_bit_to_count)
    }
    fn increment_tima(&mut self) {
        let tima = self.read(TIMA);
        if tima == 0xFF {
            let tma = self.read(TMA);
            self.write(TIMA, tma);
            let if_reg = self.read(IF) | 0x04;
            self.write(IF, if_reg);
        } else {
            self.write(TIMA, tima.wrapping_add(1));
        }
    }

    fn tick_timer_once(&mut self) {
        self.tick_div();

        let (enabled, div_bit) = self.tac_info();

        if !enabled {
            return;
        }

        let prev = ((self.div_counter.wrapping_sub(1)) >> div_bit) & 1;
        let curr = (self.div_counter >> div_bit) & 1;

        if prev == 1 && curr == 0 {
            self.increment_tima();
        }
    }

    fn advance_timer(&mut self, cycles: u8) {
        for i in 0..cycles {
            self.tick_timer_once();
        }
    }

    // ==================== ENDOF NEW AND IMPROVED FUNCS ====================

    // $CB Prefixed

    //rorates r in cirular manner to the left,7bit is copied to 0 and carry
    pub(crate) fn rlc(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let msb = value >> 7;
        let result = (value << 1) | msb;

        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, msb == 1);
    }

    pub(crate) fn rrc(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let lsb = value & 1;
        let result = (value >> 1) | (lsb << 7);

        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, lsb == 1);
    }

    // shift to the left, but discard to carry, and use carry to fill
    pub(crate) fn rl(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let carry = self.registers.get_flag(CARRY) as u8;
        let msb = value >> 7;
        let result = (value << 1) | carry;

        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, msb == 1)
    }

    pub(crate) fn rr(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let carry = self.registers.get_flag(CARRY) as u8;
        let lsb = value & 1;
        let result = (value >> 1) | (carry << 7);

        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, lsb == 1);
    }

    // same as rl but instead place 0 in the made gap
    pub(crate) fn sla(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let msb = value >> 7;
        let result = value << 1;

        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, msb == 1)
    }
    pub(crate) fn sra(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let msb = value >> 7;
        let lsb = value & 1;

        //not certain about this special case
        let result = value >> 1 | (msb << 7);

        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, lsb == 1)
    }

    pub(crate) fn swap(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let result = (value >> 4) | (value << 4);
        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, false);
    }

    pub(crate) fn srl(&mut self, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let lsb = value & 1;
        let result = value >> 1;

        self.set_operand_from_u8(op, result);
        self.update_flags(result == 0, false, false, lsb == 1);
    }

    pub(crate) fn bit(&mut self, bit_idx: Operand, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let bit_index = self.get_operand_as_u8(bit_idx);

        let bit_is_0 = (value >> bit_index) & 1 == 0;
        self.registers.set_flag(ZERO, bit_is_0);
        self.registers.set_flag(SUBSTRACTION, false);
        self.registers.set_flag(HALFCARRY, true);
    }

    pub(crate) fn res(&mut self, bit_idx: Operand, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let bit_index = self.get_operand_as_u8(bit_idx);

        let result = value & !(1 << bit_index);
        self.set_operand_from_u8(op, result);
    } //set nth bit to 0 
    pub(crate) fn set(&mut self, bit_idx: Operand, op: Operand) {
        let value = self.get_operand_as_u8(op);
        let bit_index = self.get_operand_as_u8(bit_idx);

        let result = value | (1 << bit_index);
        self.set_operand_from_u8(op, result);
    }
}

// ================================== REGISTERS =============================

pub struct Registers {
    a: u8,
    f: u8, // Flags register 4: carry 5: half_carry 6: sub 7: zero
    b: u8, // BC 16 bits
    c: u8,
    d: u8, // DE 16 bits
    e: u8,
    h: u8, // HL 16 bits
    l: u8,
    sp: u16, // Stack pointer
    pc: u16,
}
impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "A : 0x{:02X} F : 0x{:08b} \nB : 0x{:02X} C : 0x{:02X} \nD : 0x{:02X} E : 0x{:02X} \nH : 0x{:02X} L : 0x{:02X} \nSP : 0x{:04X} \nPC : 0x{:04X}",
            self.a, self.f, self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.pc
        )
    }
}
impl Registers {
    //Think about maybe using #[inline(always)]
    fn set_flag(&mut self, bit_idx: u8, flag: bool) {
        if flag {
            self.f = self.f | ((flag as u8) << bit_idx); //Bitwise or 
        } else {
            self.f = self.f & !(1 << bit_idx);
        }
    }

    fn get_flag(&self, bit_idx: u8) -> bool {
        let mask = 1 << bit_idx;
        self.f & mask != 0
    }

    fn get_16register(&self, register: Reg16) -> u16 {
        fn concat_registers(reg1: u8, reg2: u8) -> u16 {
            ((reg1 as u16) << 8) | (reg2 as u16)
        }
        match register {
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
            Reg16::AF => concat_registers(self.a, self.f),
            Reg16::BC => concat_registers(self.b, self.c),
            Reg16::DE => concat_registers(self.d, self.e),
            Reg16::HL => concat_registers(self.h, self.l),
        }
    }

    fn set_16register(&mut self, register: Reg16, value: u16) {
        fn set_registers(reg1: &mut u8, reg2: &mut u8, value: u16) {
            let (lsb, msb) = CPU::split_u16(value);

            *reg1 = msb;
            *reg2 = lsb;
        }

        match register {
            Reg16::SP => self.sp = value,
            Reg16::PC => self.pc = value,
            Reg16::AF => {
                let (lsb, msb) = CPU::split_u16(value);
                self.a = msb;
                self.f = lsb & 0xF0;
            }
            Reg16::BC => set_registers(&mut self.b, &mut self.c, value),
            Reg16::DE => set_registers(&mut self.d, &mut self.e, value),
            Reg16::HL => set_registers(&mut self.h, &mut self.l, value),
        }
    }

    fn get_u8register(&self, register: Reg8) -> u8 {
        match register {
            Reg8::A => self.a,
            Reg8::F => self.f,
            Reg8::B => self.b,
            Reg8::C => self.c,
            Reg8::D => self.d,
            Reg8::L => self.l,
            Reg8::H => self.h,
            Reg8::E => self.e,
        }
    }

    fn set_u8register(&mut self, register: Reg8, value: u8) {
        match register {
            Reg8::A => self.a = value,
            Reg8::F => self.f = value,
            Reg8::B => self.b = value,
            Reg8::C => self.c = value,
            Reg8::D => self.d = value,
            Reg8::L => self.l = value,
            Reg8::H => self.h = value,
            Reg8::E => self.e = value,
        }
    }
}
