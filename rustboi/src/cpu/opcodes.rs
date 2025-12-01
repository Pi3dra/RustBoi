use super::CPU;
use super::{FlagCondition, MemAdress, Operand, Reg8, Reg16};

#[derive(Copy, Clone, Debug)]
pub enum InstrPointer {
    Binop(fn(&mut CPU, Operand, Operand), Operand, Operand, u16),
    Unop(fn(&mut CPU, Operand), Operand, u16),
    Const(fn(&mut CPU), u16),
    None, //For non implemented funcs
}

use InstrPointer::*;
use MemAdress::*;
use Operand::*;
use Reg8::*;
use Reg16::*;

impl std::fmt::Display for InstrPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrPointer::Const(_, _) => write!(f, ""),
            InstrPointer::Binop(_, dst, src, _) => write!(f, "{:?}, {:?}", dst, src),
            InstrPointer::Unop(_, reg, _) => write!(f, "{:?}", reg),
            InstrPointer::None => write!(f, "None"),
        }
    }
}

impl CPU {
    pub(super) fn build_table() -> ([InstrPointer; 256], [InstrPointer; 256]) {
        let mut table: [InstrPointer; 256] = [None; 256]; // default NOP

        //let order : [Operand;8] = [R8(B), R8(C),R8(D),R8(E),R8(H),R8(L),Address(AddrR16(HL)),R8(A)];
        let order: [Operand; 8] = [
            R8(B),
            R8(C),
            R8(D),
            R8(E),
            R8(H),
            R8(L),
            Address(AddrR16(HL)),
            R8(A),
        ];

        let rr_order: [Operand; 4] = [R16(BC), R16(DE), R16(HL), R16(SP)];

        //let flag_order: [Operand;4] =[];

        // ========== Block 0 : 0x00 -> 0x3F ===========

        // ARITHMETIC
        //Inc Dec r
        let timing: [u16; 8] = [4, 4, 4, 4, 4, 4, 12, 4];
        let inc = Unop(CPU::inc, R8(A), 0);
        let dec = Unop(CPU::dec, R8(A), 0);
        CPU::init_instr(&mut table, inc, &order, &timing, 0x04, 0x8, false);
        CPU::init_instr(&mut table, dec, &order, &timing, 0x05, 0x8, false);

        //Add rr rr
        let timing2: [u16; 4] = [8, 8, 8, 8];
        let add_rr_rr = Unop(CPU::add_hl_rr, R8(A), 0);
        CPU::init_instr(
            &mut table, add_rr_rr, &rr_order, &timing2, 0x09, 0x10, false,
        );

        //Inc Dec rr
        let inc_rr = Unop(CPU::inc_u16, R8(A), 0);
        let dec_rr = Unop(CPU::dec_u16, R8(A), 0);
        CPU::init_instr(&mut table, inc_rr, &rr_order, &timing2, 0x03, 0x10, false);
        CPU::init_instr(&mut table, dec_rr, &rr_order, &timing2, 0x0B, 0x10, false);

        //LOADING
        //Ld r n8
        let timing3: [u16; 8] = [8, 8, 8, 8, 8, 8, 12, 8];
        let ld_r_n8 = Binop(CPU::ld_u8, R8(A), Imm8, 0);
        CPU::init_instr(&mut table, ld_r_n8, &order, &timing3, 0x06, 0x8, true);

        //Ld a addr
        let mem_order: [Operand; 4] = [
            Address(AddrR16(BC)),
            Address(AddrR16(DE)),
            Address(HLInc),
            Address(HLDec),
        ];
        let ld_a_addr = Binop(CPU::ld_u8, R8(A), R8(A), 0);
        CPU::init_instr(
            &mut table, ld_a_addr, &mem_order, &timing2, 0x0A, 0x10, false,
        );

        //Ld addr a
        let mem_order: [Operand; 4] = [
            Address(AddrR16(BC)),
            Address(AddrR16(DE)),
            Address(HLInc),
            Address(HLDec),
        ];
        let ld_addr_a = Binop(CPU::ld_u8, R8(A), R8(A), 0);
        CPU::init_instr(
            &mut table, ld_addr_a, &mem_order, &timing2, 0x02, 0x10, true,
        );

        //Ld rr n16
        let timing4: [u16; 4] = [12, 12, 12, 12];
        let ld_rr_n16 = Binop(CPU::ld_u16, R8(A), Imm16, 0);
        CPU::init_instr(&mut table, ld_rr_n16, &rr_order, &timing4, 0x01, 0x10, true);

        //RELATIVE JUMPS
        let jr_order: [Operand; 5] = [
            Flag(FlagCondition::None),
            Flag(FlagCondition::NZ),
            Flag(FlagCondition::Z),
            Flag(FlagCondition::NC),
            Flag(FlagCondition::C),
        ];
        let jr_timing: [u16; 5] = [0, 0, 0, 0, 0]; //Jr is conditional, timing is handled in func
        let jr_cond_e8 = Binop(CPU::jr, R8(A), Imm8, 0);
        CPU::init_instr(
            &mut table, jr_cond_e8, &jr_order, &jr_timing, 0x18, 0x08, true,
        );

        //SPECIAL CASES
        table[0x00] = Const(CPU::nop, 4);
        table[0x10] = Const(CPU::stop, 4);

        table[0x07] = Const(CPU::rlca, 4);
        table[0x17] = Const(CPU::rla, 4);
        table[0x27] = Const(CPU::daa, 4);
        table[0x37] = Const(CPU::scf, 4);

        table[0x0F] = Const(CPU::rrca, 4);
        table[0x1F] = Const(CPU::rra, 4); //Not implemented!
        table[0x2F] = Const(CPU::cpl, 4); //Not implemented!
        table[0x3F] = Const(CPU::ccf, 4);

        table[0x08] = Binop(CPU::ld_u16, Address(ImmAddr16), R16(SP), 20);

        // ========== Block 1 : 0x40 -> 0x7F ===========

        for (i, operand) in order.iter().enumerate() {
            let ld_instr = Binop(CPU::ld_u8, *operand, R8(A), 0);
            let start: u8 = 0x40 + (i as u8) * 0x08;

            let ld_timing = if matches!(*operand, Address(AddrR16(HL))) {
                [8, 8, 8, 8, 8, 8, 8, 8]
            } else {
                [4, 4, 4, 4, 4, 4, 8, 4]
            };

            CPU::init_instr(&mut table, ld_instr, &order, &ld_timing, start, 0x01, false);
        }

        table[0x76] = Const(CPU::halt, 4);

        // ========== Block 2 : 0x80 -> 0xBF ===========

        let arith_funcs = [
            CPU::add,
            CPU::adc,
            CPU::sub,
            CPU::sbc,
            CPU::and,
            CPU::xor,
            CPU::or,
            CPU::cp,
        ];
        for (i, func) in arith_funcs.iter().enumerate() {
            let arith_instr = Binop(*func, R8(A), R8(A), 0);
            let start: u8 = 0x80 + (i as u8) * 0x08;
            let timing = [4, 4, 4, 4, 4, 4, 8, 4];
            CPU::init_instr(&mut table, arith_instr, &order, &timing, start, 0x01, false);
        }

        // ========== Block 3 : 0xC0 -> 0xFF ===========

        // POP PUSH
        let pop_timing: [u16; 4] = [12; 4];
        let push_timing: [u16; 4] = [16; 4];

        //pop rr , push rr
        let pop = Unop(CPU::pop, R8(A), 0);
        let push = Unop(CPU::push, R8(A), 0);

        let pop_order: [Operand; 4] = [R16(BC), R16(DE), R16(HL), R16(AF)];
        CPU::init_instr(&mut table, pop, &pop_order, &pop_timing, 0xC1, 0x10, false);
        CPU::init_instr(
            &mut table,
            push,
            &pop_order,
            &push_timing,
            0xC5,
            0x10,
            false,
        );

        //ARITHMETIC
        for (i, func) in arith_funcs.iter().enumerate() {
            let opcode = 0xC6 + i * 0x08 as usize;
            table[opcode] = Binop(*func, R8(A), Imm8, 8);
        }

        //JUMPS N CALLS

        //rst
        let rst_order: [Operand; 8] = [
            Value(0x00),
            Value(0x08),
            Value(0x10),
            Value(0x18),
            Value(0x20),
            Value(0x28),
            Value(0x30),
            Value(0x38),
        ];
        let rst_timing: [u16; 8] = [16; 8];
        let rst = Unop(CPU::rst, R8(A), 0);
        CPU::init_instr(&mut table, rst, &rst_order, &rst_timing, 0xC7, 0x08, false);

        // Ret cond, Jp cond, call cond
        let call_order: [Operand; 4] = [
            Flag(FlagCondition::NZ),
            Flag(FlagCondition::Z),
            Flag(FlagCondition::NC),
            Flag(FlagCondition::C),
        ];
        let call_timing: [u16; 4] = [0; 4]; //conditional handle clock themselves;

        let ret = Unop(CPU::ret, R8(A), 0);
        let jp = Binop(CPU::jp, R8(A), Imm16, 0);
        let call = Binop(CPU::call, R8(A), Imm16, 0);

        CPU::init_instr(
            &mut table,
            ret,
            &call_order,
            &call_timing,
            0xC0,
            0x08,
            false,
        );
        CPU::init_instr(&mut table, jp, &call_order, &call_timing, 0xC2, 0x08, true);
        CPU::init_instr(
            &mut table,
            call,
            &call_order,
            &call_timing,
            0xC4,
            0x08,
            true,
        );

        //Inconditionals ret, jp, call
        let inconditional = Flag(FlagCondition::None);
        table[0xC9] = Unop(CPU::ret, inconditional, 0);
        table[0xD9] = Const(CPU::reti, 0);
        table[0xC3] = Binop(CPU::jp, inconditional, Imm16, 0);
        table[0xE9] = Binop(CPU::jp, inconditional, R16(HL), 0);
        table[0xCD] = Binop(CPU::call, inconditional, Imm16, 0);

        //Special ones
        table[0xE8] = Const(CPU::add_sp_e8, 16);
        table[0xF8] = Binop(CPU::ld_u16_e8, R16(HL), R16(SP), 12); //ld hl sp+e8
        table[0xF9] = Binop(CPU::ld_u16, R16(SP), R16(HL), 8); //ld hl sp+e8

        table[0xF3] = Const(CPU::di, 4);
        table[0xFB] = Const(CPU::ei, 4);

        //ldh
        table[0xE0] = Binop(CPU::ld_u8, Address(ImmAddr8), R8(A), 8);
        table[0xF0] = Binop(CPU::ld_u8, R8(A), Address(ImmAddr8), 8);
        table[0xE2] = Binop(CPU::ld_u8, Address(AddrR8(C)), R8(A), 8);
        table[0xF2] = Binop(CPU::ld_u8, R8(A), Address(AddrR8(C)), 8);

        table[0xEA] = Binop(CPU::ld_u8, Address(ImmAddr16), R8(A), 16);
        table[0xFA] = Binop(CPU::ld_u8, R8(A), Address(ImmAddr16), 16);

        // ========== CB Prefixed ===========

        let mut cb_table: [InstrPointer; 256] = [None; 256];

        let func_order = [
            CPU::rlc,
            CPU::rrc,
            CPU::rl,
            CPU::rr,
            CPU::sla,
            CPU::sra,
            CPU::swap,
            CPU::srl,
        ];
        let cb_timing1: [u16; 8] = [8, 8, 8, 8, 8, 8, 16, 8];
        let cb_timing2: [u16; 8] = [8, 8, 8, 8, 8, 8, 12, 8];
        for (i, func) in func_order.iter().enumerate() {
            let start = 0x00 + i * 0x08;
            CPU::init_instr(
                &mut cb_table,
                Unop(*func, R8(A), 0),
                &order,
                &cb_timing1,
                start as u8,
                0x01,
                false,
            );
        }

        //weird error here if i didn't use the "as ..."
        let cb_ops = [
            (
                CPU::bit as fn(&mut CPU, Operand, Operand),
                0x40,
                &cb_timing2,
            ),
            (
                CPU::res as fn(&mut CPU, Operand, Operand),
                0x80,
                &cb_timing1,
            ),
            (
                CPU::set as fn(&mut CPU, Operand, Operand),
                0xC0,
                &cb_timing1,
            ),
        ];

        for (instr, base_offset, timing) in cb_ops.iter() {
            for i in 0..8 {
                let start = base_offset + (i * 0x08) as u8;
                CPU::init_instr(
                    &mut cb_table,
                    Binop(*instr, Value(i as u16), R8(A), 0),
                    &order,
                    *timing,
                    start,
                    0x01,
                    false,
                );
            }
        }

        (table, cb_table)
    }

    fn init_instr(
        table: &mut [InstrPointer; 256],
        instr: InstrPointer,
        order: &[Operand],
        timing: &[u16],
        start: u8,
        offset: u8,
        replace_left: bool,
    ) {
        let inc_data = CPU::produce_metadata(order, timing, start, offset);
        for (opcode, operand, cycles) in inc_data {
            let built_instr = CPU::build_instrpointer(instr, operand, cycles, replace_left);
            assert!(
                matches!(table[opcode as usize], None),
                "Opcode conflict at 0x{:02X} between the already existing func '{:?}' and '{:?}' collides with existing instruction",
                opcode,
                table[opcode as usize],
                built_instr,
            );
            table[opcode as usize] = built_instr;
        }
    }

    fn produce_metadata(
        operand_order: &[Operand],
        clock_mapping: &[u16],
        start: u8,
        offset: u8,
    ) -> Vec<(u8, Operand, u16)> {
        /* Produces the necessary metadata for building the opcode table properly -> (opcode, operand,
         * clock_cycles)
         **/
        assert_eq!(operand_order.len(), clock_mapping.len());

        let mut metadata = Vec::new();

        let iterator = operand_order.iter().zip(clock_mapping.iter()).enumerate();
        for (i, (operand, clock_cycles)) in iterator {
            let opcode = start + (i as u8) * offset;
            metadata.push((opcode, *operand, *clock_cycles));
        }
        metadata
    }

    fn build_instrpointer(
        func: InstrPointer,
        given_op: Operand,
        clock_cycles: u16,
        replace_left: bool,
    ) -> InstrPointer {
        /* Takes a half built instrpointer and replaces the righthand side of a Binop, and the
         * instruction, then returns that
         * */
        match func {
            Binop(f, lop, rop, _) => {
                if replace_left {
                    return Binop(f, given_op, rop, clock_cycles);
                } else {
                    return Binop(f, lop, given_op, clock_cycles);
                }
            }
            Unop(f, _, _) => Unop(f, given_op, clock_cycles),
            Const(f, _) => Const(f, clock_cycles),
            None => panic!("Encoding none function"),
        }
    }
}
