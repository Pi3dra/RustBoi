pub mod cpu;
pub mod opcodes;

#[derive(Clone, Copy, Debug)]
pub(crate) enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[allow(dead_code)] // F is never constructed but we do use it!
#[derive(Clone, Copy, Debug)]
pub(crate) enum Reg8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[allow(dead_code)] // for Fixed(u16)
#[derive(Copy, Clone, Debug)]
pub(crate) enum MemAdress {
    //memory is u8!
    HLInc,          //[HL+]
    HLDec,          //[HL-]
    AddrR8(Reg8),   //[A]
    AddrR16(Reg16), //[SP]
    ImmAddr8,       //[a8]
    ImmAddr16,      //[a16]
    Fixed(u16),     // $18
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum FlagCondition {
    NZ,   // Not Zero
    Z,    // Zero
    NC,   // Not Carry
    C,    // Carry
    None, // Unconditional (for JR e, CALL nn, etc.)
}

#[derive(Copy, Clone)]
pub(crate) enum Operand {
    R8(Reg8),
    R16(Reg16),
    Address(MemAdress),
    Flag(FlagCondition),
    Imm8,
    Imm16,
    Value(u16), //Special for rst function
}

impl std::fmt::Debug for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::R8(reg) => write!(f, "{:?}", reg),
            Operand::R16(reg) => write!(f, "{:?}", reg),
            Operand::Address(addr) => match addr {
                MemAdress::HLInc => write!(f, "[HL+]"),
                MemAdress::HLDec => write!(f, "[HL-]"),
                MemAdress::AddrR8(reg) => write!(f, "[{:?}]", reg),
                MemAdress::AddrR16(reg) => write!(f, "[{:?}]", reg),
                MemAdress::ImmAddr8 => write!(f, "[a8]"),
                MemAdress::ImmAddr16 => write!(f, "[a16]"),
                MemAdress::Fixed(val) => write!(f, "${:04X}", val),
            },
            Operand::Flag(flag) => match flag {
                FlagCondition::NZ => write!(f, "NZ"),
                FlagCondition::Z => write!(f, "Z"),
                FlagCondition::NC => write!(f, "NC"),
                FlagCondition::C => write!(f, "C"),
                FlagCondition::None => write!(f, "None"),
            },
            Operand::Imm8 => write!(f, "Imm8"),
            Operand::Imm16 => write!(f, "Imm16"),
            Operand::Value(val) => write!(f, "{}", val),
        }
    }
}

pub use cpu::CPU;
