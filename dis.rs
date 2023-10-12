// Copyright © 2023 David Caldwell <david@porkrind.org>

use std::{error::Error, io::{Read, Write, BufReader}, collections::HashMap};

use crate::cdf8::*;

pub fn disassemble<H,R,W>(hw: &H, input: &R, output: &mut W) -> Result<(), Box<dyn Error>>
where for<'a> &'a R: Read,
                  W: Write,
                  H: DecodeInstruction,
{
    let reader = BufReader::new(input);
    let mut byte = reader.bytes();
    let mut program: Vec<Instruction> = Vec::new();
    let mut raw: Vec<u16> = Vec::new();
    loop {
        let word = match (byte.next(), byte.next()) {
            (Some(Ok(high)),  Some(Ok(low))) => (high as u16) << 8 | low as u16,
            (Some(Err(e)),    _            ) |
            (_,               Some(Err(e)) ) => Err(e)?,
            (Some(_),         None         ) => Err(format!("Input has odd number of bytes"))?,
            (None,            _            ) => break/* normal end*/,
        };

        let insn = hw.decode(word).map_err(|e| format!("In instruction {:016b} @ {:#o}: {}", word, program.len(), e))?;
        program.push(insn);
        raw.push(word);
    }
    let mut symbols = SymbolTable::new();
    if hw.variant() == ProcessorVariant::OneBoard {
        // The TwoBoard hardware has 2 banks of ROMs (which they call "fields"). The jump labelling isn't
        // compatible with the fields (needs detection of the "F CDF[01]" instructions). I'm not going to
        // implement that since we already have an actual printout of the TwoBoard microcode source (with
        // comments!), and since we don't actually have any TwoBoard hardware anyway.
        label_jumps(&mut program, &mut symbols);
    }
    for (addr, insn) in program.iter().enumerate() {
        writeln!(&mut *output, "{label:<10} {:3o} {:016b} {}", addr, raw[addr], insn.disassemble(addr as u16)?,
                 label=match symbols.symbol_for_addr(addr as u16) { Some(name) =>  format!("{}:", name), _ => format!("") })?;
    }
    Ok(())
}

struct SymbolTable {
    symbols: HashMap<String, u16>,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable { symbols: HashMap::new() }
    }
    fn symbol_for_addr(&self, addr: u16) -> Option<String> {
        // We could index or cache this but who cares, the rom only holds like 500 instructions
        if let Some((name, _)) = self.symbols.iter().find(|(_, a)| **a == addr) {
            Some(name.clone())
        } else {
            None
        }
    }
    fn addr_for_symbol(&self, name: &str) -> Option<u16> {
        self.symbols.get(name).copied()
    }
    fn insert(&mut self, name: String, addr: u16) {
        self.symbols.insert(name, addr);
    }
}

fn label_jumps(program: &mut Vec<Instruction>, symbols: &mut SymbolTable) {
    for (addr, insn) in program.iter_mut().enumerate() {
        match insn {
            Instruction::Jump{ when, condition, effective_address: JumpDest::Absolute(a) } if *a != addr as u16 => {
                if symbols.symbol_for_addr(*a).is_none() {
                    symbols.insert(format!("addr{:o}", a), *a);
                }
                *insn = Instruction::Jump{ when: *when, condition: *condition, effective_address: JumpDest::Symbol(symbols.symbol_for_addr(*a).unwrap().clone()) };
            },
            _ => {},
        }
    }
}


impl Instruction {
    pub fn disassemble(&self, addr: u16) -> Result<String, Box<dyn Error>> {
        Ok(match self {
            Instruction::Jump { when: true, condition: Condition::NoOperation, effective_address: JumpDest::Absolute(0) } => {
                format!("NOP")
            },
            Instruction::Jump { when: false, condition: Condition::NoOperation, effective_address } => {
                format!("{:<8} {}", "JUMP", effective_address)
            },
            Instruction::Jump { when: false, condition, effective_address: JumpDest::Absolute(effective_address) } if *effective_address == addr => {
                format!("{:<8} {}", "WAIT", condition)
            },
            Instruction::Jump { when, condition, effective_address } => {
                format!("{:<8} {},{}", if *when { "JTC" } else { "JFC" }, effective_address, condition)
            },
            Instruction::FunctionALU { alu: None, function } => {
                format!("{:<8} {}", "F", function)
            },
            Instruction::FunctionALU { alu: Some(mode), function: Function::NOP } => {
                format!("{:<8} ,{}", "F", mode)
            },
            Instruction::FunctionALU { alu: Some(mode), function } => {
                format!("{:<8} {},{}", "F", function, mode)
            },
            Instruction::FunctionTimer { timer, function } => {
                let f = match function {
                    Function::NOP => format!(""),
                    f => format!("{}", f),
                };
                format!("{:<8} {},{:o}{}", "CF", f, 256u16 - timer.negative_count as u16, timer.clock_rate)
            },
            Instruction::Move { source, dest } => {
                    let (src, src_comment) = match source {
                        MoveSource::Literal(byte)                       => (format!("#{:o}", byte), None),
                        MoveSource::Register(SourceRegister::GPReg(gp)) => (format!("GP{}", gp), None),
                        MoveSource::Register(reg)                       => (format!("{}", reg), None),
                        MoveSource::Constant(c)                         =>{ // Assemble a bit of the raw instruction back for the comment (to match old assembler)
                                                                            let raw = 0b100_0000 | if *c < 16 {*c} else {*c & 0b1111 | 0b10_0000};
                                                                            let val = CONSTANT_ROM[*c as usize];
                                                                            (format!("#{:o}", val), Some(format!("constant[{:o}] (bank {} #{}) raw={:o} ({:b})", c, c >> 4, c & 0b1111, raw, raw))) },
                    };
                    let (dst, dst_comment) = match dest {
                        DestRegister::GPReg(gp)                         => (format!("GP{}", gp), None),
                        reg                                             => (format!("{}", reg), None),
                    };
                let dis = format!("{:<8} {},{}", "MV", src, dst);
                match (&src_comment, &dst_comment) {
                    (None, None) => dis,
                    (_,    _)    => format!("{:<24}; {},{}", dis, src_comment.unwrap_or("".to_string()), dst_comment.unwrap_or("".to_string())),
                }
            },
        })
    }
}

#[derive(PartialEq)]
pub enum ProcessorVariant {
    TwoBoard, // Old
    OneBoard, // New
}

pub trait DecodeInstruction {
    fn variant(&self) -> ProcessorVariant;
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>>;
}

pub struct TwoBoard {
}

impl DecodeInstruction for TwoBoard {
    fn variant(&self) -> ProcessorVariant { ProcessorVariant::TwoBoard }
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(match RawOpcodeTwoBoard::from_repr(word & OPCODE_MASK).unwrap() {
            RawOpcodeTwoBoard::Jump          => self.decode_jump(word)?,
            RawOpcodeTwoBoard::FunctionTimer => self.decode_funct_timer(word)?,
            RawOpcodeTwoBoard::FunctionALU   => self.decode_funct_alu(word)?,
            RawOpcodeTwoBoard::Move          => self.decode_move(word)?,
        })
    }
}

impl TwoBoard {
    pub fn new() -> TwoBoard { TwoBoard{} }

    fn decode_jump(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        let condition = (word & 0b0001_1111_0000_0000) >> 8;
        Ok(Instruction::Jump {
            when: (word & 0b0010_0000_0000_0000) == 0,
            condition: match condition {
                // Special cases where TwoBoard is different from OneBoard
                0o2  => Condition::MemoryReady,
                0o12 => Condition::SectorHeaderMark,
                0o13 => Condition::DataId,
                0o14 => Condition::DelData,
                0o16 => Condition::FileInop,
                // These are the same as OneBoard
                _    => Condition::from_repr(condition as u8).ok_or_else(|| format!("Unknown condition {} {:#07b} {:#02o}", condition, condition, condition))?
            },
            effective_address: JumpDest::Absolute(word & 0b0000_0000_1111_1111)
        })
    }

    fn decode_function(&self, word: u16) -> Result<Function, Box<dyn Error>> {
        let func = ((word & 0b00_11111_0_0000_0000) >> 9) as u8;
        Ok(Function::from_repr(func).ok_or_else(|| format!("Unknown function {} {:#07b} {:#2o}", func, func, func))?)
    }

    fn decode_funct_timer(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(Instruction::FunctionTimer { function: self.decode_function(word)?,
                                        timer:    Timer { negative_count: (word & 0xff) as u8,
                                                          clock_rate: match (word & 0b0000_0001_0000_0000) != 0 {
                                                              false => ClockRate::Millisecond,
                                                              true  => ClockRate::Microsecond
                                                          } }
        })
    }

    fn decode_funct_alu(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        let function = self.decode_function(word)?;
        Ok(match (word & 0b0000_0000_0010_0000) != 0 {
            false => Instruction::FunctionALU   { function, alu: None },
            true  => Instruction::FunctionALU   { function, alu: Some(
                match word & 0xF { // On the TwoBoard version these 4 bits go through a PROM that outputs the 6 bits to the ALU (C+M+S4-0)
                    0o0  => ALUMode::PLUS,
                    0o1  => ALUMode::MINUS,
                    0o2  => ALUMode::DEC,
                    0o3  => ALUMode::INC,
                    0o4  => ALUMode::ROL,
                    0o5  => ALUMode::NOTA,
                    0o6  => ALUMode::NOTB,
                    0o7  => ALUMode::NOR,
                    0o10 => ALUMode::NAND,
                    0o11 => ALUMode::XOR,
                    0o12 => ALUMode::XORBAR,
                    0o13 => ALUMode::AND,
                    0o14 => ALUMode::OR,
                    0o15 => ALUMode::CPYA,
                    0o16 => ALUMode::CPYB,
                    0o17 => ALUMode::CMP,
                    _ => unreachable!(),
                })
            },
        })
    }

    fn decode_move(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(Instruction::Move{ // This is also fairly different
            source: match ((word & 0b0010_0000_0000_0000)!=0, // Constant Mode
                           (word & 0b0001_0000_0000_0000)!=0, // Group 2 Constants
                           (word & 0b0000_1000_0000_0000)!=0, // Data Path/GP
                           (word & 0b0000_0111_1000_0000)>>7) { // Source Address
                               (true,  group2,  false, addr)   => MoveSource::Constant(if group2 { 0b1_0000 } else { 0 } | addr as u8),
                               (false, _,       false, reg)    => MoveSource::Register(SourceRegister::from_repr(reg as u8).ok_or_else(|| format!("Unknown source register: {}, {:#b} {:#o}", reg, reg, reg))?),
                               (false, _,       true,  gp_reg) => MoveSource::Register(SourceRegister::GPReg(gp_reg as u8)),
                               (true,  _,       true,  _)      => Err(format!("Can't set Data Path/GP and Constant Mode bits at the same time"))?,
            },
            dest: match ((word & MOVE_DEST_GP)!=0, word & MOVE_DEST_REG) {
                (false, reg)    => DestRegister::from_repr(reg as u8).ok_or_else(|| format!("Unknown destination register: {}, {:#b} {:#o}", reg, reg, reg))?,
                (true,  gp_reg) => DestRegister::GPReg(gp_reg as u8),
            }
        })
    }
}

pub struct OneBoard {
}

impl DecodeInstruction for OneBoard {
    fn variant(&self) -> ProcessorVariant { ProcessorVariant::OneBoard }
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(match RawOpcodeOneBoard::from_repr(word & OPCODE_MASK).unwrap() {
            RawOpcodeOneBoard::JumpTrue         => self.decode_jump(true, word)?,
            RawOpcodeOneBoard::JumpFalse        => self.decode_jump(false, word)?,
            RawOpcodeOneBoard::FunctionALUTimer => self.decode_funct_alu_timer(word)?,
            RawOpcodeOneBoard::Move             => self.decode_move(word)?,
        })
    }
}

impl OneBoard {
    pub fn new() -> OneBoard { OneBoard{} }
    fn decode_jump(&self, when: bool, word: u16) -> Result<Instruction, Box<dyn Error>> {
        let condition = (word & 0b00_11111_0_0000_0000) >> 9;
        Ok(Instruction::Jump {
            when,
            condition: Condition::from_repr(condition as u8).ok_or_else(|| format!("Unknown condition {} {:#07b} {:#02o}", condition, condition, condition))?,
            effective_address: JumpDest::Absolute(word & 0b00_00000_1_1111_1111)
        })
    }

    fn decode_funct_alu_timer(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        let func = ((word & 0b00_11111_0_0000_0000) >> 9) as u8;
        let function = Function::from_repr(func).ok_or_else(|| format!("Unknown function {} {:#07b} {:#2o}", func, func, func))?;
        Ok(match ((word & TIMER_ALU_SELECT) == TIMER_ALU_SELECT_TIMER,
                  (word & ALU_FUNCT_SELECT) == ALU_FUNCT_SELECT_ALU) {
            (true,  _    ) => Instruction::FunctionTimer { function, timer: self.decode_timer(word)? },
            (false, true ) => Instruction::FunctionALU   { function, alu: Some(self.decode_alu(word)?) },
            (false, false) => Instruction::FunctionALU   { function, alu: None },
        })
    }

    fn decode_timer(&self, word: u16) -> Result<Timer, Box<dyn Error>> {
        Ok(Timer { negative_count: (word & 0xff) as u8, clock_rate: ClockRate::Millisecond })
    }

    fn decode_alu(&self, word: u16) -> Result<ALUMode, Box<dyn Error>> {
        let nc_m_s3210 = (word & 0b11_1111) as u8;
        Ok(ALUMode::from_repr(nc_m_s3210).ok_or_else(|| format!("Unknown ALU bits: !C={} M={}, S[3..0]={}", nc_m_s3210 >> 5, (nc_m_s3210 >> 4) & 1, nc_m_s3210 & 0b1111))?)
    }

    fn decode_move(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(Instruction::Move{
            source: match ((word & MOVE_SOURCE_LITERAL)!=0, (word & MOVE_SOURCE_LITERAL_MASK) >> 5, (word & MOVE_SOURCE_GP)!=0, (word & MOVE_SOURCE_REG) >> 5) {
                (true,  literal, _,     _)      => MoveSource::Literal(literal as u8),
                (false, _,       false, reg)    => MoveSource::Register(SourceRegister::from_repr(reg as u8).ok_or_else(|| format!("Unknown source register: {}, {:#b} {:#o}", reg, reg, reg))?),
                (false, _,       true,  gp_reg) => MoveSource::Register(SourceRegister::GPReg(gp_reg as u8)),
            },
            dest: match ((word & MOVE_DEST_GP)!=0, word & MOVE_DEST_REG) {
                (false, reg)    => DestRegister::from_repr(reg as u8).ok_or_else(|| format!("Unknown destination register: {}, {:#b} {:#o}", reg, reg, reg))?,
                (true,  gp_reg) => DestRegister::GPReg(gp_reg as u8),
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn two_board_alu_rom() {
        let two_board = TwoBoard::new();
        let alu_rom: [u8; 16] = [
            // Actual ALU ROM dump. Top 2 bits were unused. This translates the 4 bits of the instruction into
            // 6 bits of ALU inputs. The top 2 bits are unused and remained as unprogrammed 1s in the PROM.
            // Specifically, the output mappings are:
            //XXCM_0123     C=Carry(Bar), M=M (logic mode), 0-4=S0-S4.
            0b1110_1001,
            0b1100_0110,
            0b1110_1111,
            0b1100_0000,
            0b1110_0011, //0b1110_1100 in the dump printout, but there's a handwritten "wrong" next to it :-)
            0b1111_0000,
            0b1111_1010,
            0b1111_1000,
            0b1111_0010,
            0b1111_0110,
            0b1111_1001,
            0b1111_1101,
            0b1111_0111,
            0b1111_1111,
            0b1111_0101,
            0b1110_0110,
        ];

        let reverse: [u8; 16] = [ // look up table for bit-reversing a nibble
            0b0000, 0b1000, 0b0100, 0b1100, 0b0010, 0b1010, 0b0110, 0b1110,
            0b0001, 0b1001, 0b0101, 0b1101, 0b0011, 0b1011, 0b0111, 0b1111,
        ];

        for (i, rom_entry) in alu_rom.into_iter().enumerate() {
            let rom_entry = rom_entry & 0b0011_1111; // Top two bits aren't used

            // Output of ALU ROM has S3 as the LSB, then S2, S1, S0. This is reversed from the one-board
            // revision where the bits come right out of the instruction. The main code is from the one-board
            // perspective so to validate we have to bit reverse the lower nibble of the ROM.
            let rom_entry = rom_entry & 0b0011_0000 | reverse[(rom_entry & 0b0000_1111) as usize];
            let word = RawOpcodeTwoBoard::FunctionALU as u16 | 0b10_0000 | i as u16;
            let Instruction::FunctionALU { function:_, alu: Some(mode) } = two_board.decode_funct_alu(word).expect("decode failed")
                else { panic!("Instruction encode failed for {:160b}", word) };
            if mode as u8 != rom_entry {
                panic!("alu_rom[{i}] != decode({insn:016b})\nalu_rom: {rom:08b}\ndecode : {modeu8:08b} {mode}", i=i, rom=rom_entry, insn=word, modeu8=mode as u8, mode=mode);
            }
            assert_eq!(mode as u8, rom_entry);
        };
    }
}
