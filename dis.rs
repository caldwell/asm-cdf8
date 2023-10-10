// Copyright © 2023 David Caldwell <david@porkrind.org>

use std::{error::Error, io::{Read, Write, BufReader}};

use crate::cdf8::*;

pub fn disassemble<H,R,W>(hw: &H, input: &R, output: &mut W) -> Result<(), Box<dyn Error>>
where for<'a> &'a R: Read,
                  W: Write,
                  H: DecodeInstruction,
{
    let reader = BufReader::new(input);
    let mut byte = reader.bytes();
    let mut addr = 0;
    loop {
        let word = match (byte.next(), byte.next()) {
            (Some(Ok(high)),  Some(Ok(low))) => (high as u16) << 8 | low as u16,
            (Some(Err(e)),    _            ) |
            (_,               Some(Err(e)) ) => Err(e)?,
            (Some(_),         None         ) => Err(format!("Input has odd number of bytes"))?,
            (None,            _            ) => break/* normal end*/,
        };

        let insn = hw.decode(word).map_err(|e| format!("In instruction {:016b} @ {:#o}: {}", word, addr, e))?;

        writeln!(&mut *output, "{:3o} {:016b} {}", addr, word, insn.disassemble(addr)?)?;
        addr += 1;
    }
    Ok(())
}

impl Instruction {
    pub fn disassemble(&self, addr: u16) -> Result<String, Box<dyn Error>> {
        Ok(match self {
            Instruction::Jump { when: true, condition: Condition::NoOperation, effective_address: 0 } => {
                format!("NOP")
            },
            Instruction::Jump { when: false, condition: Condition::NoOperation, effective_address } => {
                format!("{:<8} {:o}", "JUMP", effective_address)
            },
            Instruction::Jump { when: false, condition, effective_address } if *effective_address == addr => {
                format!("{:<8} {}", "WAIT", condition)
            },
            Instruction::Jump { when, condition, effective_address } => {
                format!("{:<8} {:o},{}", if *when { "JTC" } else { "JFC" }, effective_address, condition)
            },
            Instruction::FunctionALU { alu: None, function } => {
                format!("{:<8} {}", "F", function)
            },
            Instruction::FunctionALU { alu: Some(mode), function: Function::NOP } => {
                format!("{:<8} ,{}", "F", mode.mode)
            },
            Instruction::FunctionALU { alu: Some(mode), function } => {
                format!("{:<8} {},{}", "F", function, mode.mode)
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
                        MoveSource::Register(SourceRegister::GPReg(gp)) => (format!("GP{:o}", gp), None),
                        MoveSource::Register(reg)                       => (format!("{}", reg), None),
                        MoveSource::Constant(c)                         =>{ // Assemble a bit of the raw instruction back for the comment (to match old assembler)
                                                                            let raw = 0b100_0000 | if *c < 16 {*c} else {*c & 0b1111 | 0b10_0000};
                                                                            (format!("%{:o}", c), Some(format!("{:o} ({:b}) [bank {} #{}]", raw, raw, c >> 4, c & 0b1111))) },
                    };
                    let (dst, dst_comment) = match dest {
                        DestRegister::GPReg(gp)                         => (format!("GP{:o}", gp), None),
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


pub trait DecodeInstruction {
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>>;
}

pub struct TwoBoard {
}

impl DecodeInstruction for TwoBoard {
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(match RawOpcodeTwoBoard::from_repr(word & OPCODE_MASK).unwrap() {
            RawOpcodeTwoBoard::Jump          => self.decode_jump(word)?,
            RawOpcodeTwoBoard::FunctionTimer => self.decode_funct_timer(word)?,
            RawOpcodeTwoBoard::FunctionAlu   => self.decode_funct_alu(word)?,
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
                0o2  => Condition::MemoryReady,
                0o12 => Condition::SectorHeaderMark,
                0o13 => Condition::DataId,
                0o14 => Condition::DelData,
                0o16 => Condition::FileInop,
                _    => Condition::from_repr(condition as u8).ok_or_else(|| format!("Unknown condition {} {:#07b} {:#02o}", condition, condition, condition))?
            },
            effective_address: (word & 0b0000_0000_1111_1111)
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
            true  => Instruction::FunctionALU   { function, alu: Some(ALU {
                mode: match word & 0xF {
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
                }})
            },
        })
    }

    fn decode_move(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(Instruction::Move{
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
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(match RawOpcodeOneBoard::from_repr(word & OPCODE_MASK).unwrap() {
            RawOpcodeOneBoard::JumpTrue         => self.decode_jump(true, word)?,
            RawOpcodeOneBoard::JumpFalse        => self.decode_jump(false, word)?,
            RawOpcodeOneBoard::FunctionAluTimer => self.decode_funct_alu_timer(word)?,
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
            effective_address: (word & 0b00_00000_1_1111_1111)
        })
    }

    fn decode_funct_alu_timer(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        let func = ((word & 0b00_11111_0_0000_0000) >> 9) as u8;
        let function = Function::from_repr(func).ok_or_else(|| format!("Unknown function {} {:#07b} {:#2o}", func, func, func))?;
        Ok(match (word & TIMER_ALU_SELECT, word & ALU_FUNCT_SELECT) {
            (TIMER_ALU_SELECT_TIMER, 0)                    => Instruction::FunctionTimer { function, timer: self.decode_timer(word)? },
            (0                     , ALU_FUNCT_SELECT_ALU) => Instruction::FunctionALU   { function, alu: Some(self.decode_alu(word)?) },
            (0                     , 0)                    => Instruction::FunctionALU   { function, alu: None },
            _ => Err(format!("ALU and Timer bits both set in function instruction"))?,
        })
    }

    fn decode_timer(&self, word: u16) -> Result<Timer, Box<dyn Error>> {
        Ok(Timer { negative_count: (word & 0xff) as u8, clock_rate: ClockRate::Millisecond })
    }

    fn decode_alu(&self, word: u16) -> Result<ALU, Box<dyn Error>> {
        let nc_m_s3210 = (word & 0b11_1111) as u8;
        Ok(ALU { mode: ALUMode::from_repr(nc_m_s3210).ok_or_else(|| format!("Unknown ALU bits: !C={} M={}, S[3..0]={}", nc_m_s3210 >> 5, (nc_m_s3210 >> 4) & 1, nc_m_s3210 & 0b1111))? })
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
