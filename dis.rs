// Copyright © 2023 David Caldwell <david@porkrind.org>

use strum_macros::FromRepr;

use std::{error::Error, io::{Read, Write, BufReader}};

const TIMER_ALU_SELECT:       u16 = 0b0000_0001_0000_0000;
const TIMER_ALU_SELECT_TIMER: u16 = 0b0000_0001_0000_0000;
const ALU_FUNCT_SELECT:       u16 = 0b0000_0000_1000_0000;
const ALU_FUNCT_SELECT_ALU:   u16 = 0b0000_0000_1000_0000;

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

        writeln!(&mut *output, "{:3o} {:016b} {:?}", addr, word, insn)?;
        addr += 1;
    }
    Ok(())
}

#[derive(Debug,Clone)]
pub enum Instruction {
    Jump{ when: bool, condition: Condition, effective_address: u16 },
    FunctionTimer { timer: Timer, function: Function },
    FunctionALU { alu: Option<ALU>, function: Function },
    Move{ source: MoveSource, dest: DestRegister },
}

#[derive(Debug,Clone)]
pub struct Timer {
    pub negative_count: u8,
    pub clock_rate: ClockRate,
}

#[derive(Debug,Clone)]
pub enum ClockRate {
    Millisecond,
    Microsecond,
}

#[derive(Debug,Clone)]
pub struct ALU {
    pub mode: ALUMode,
}

#[derive(Debug,Clone)]
pub enum MoveSource {
    Literal(u8),
    Register(SourceRegister),
    Constant(u8),
}

#[derive(FromRepr, Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum SourceRegister {
    #[strum(serialize = "DISKIN")]  RWFromDSU = 0,
    #[strum(serialize = "STATUS")]  ErrorStatReg = 1,
    #[strum(serialize = "DATAIN")]  DataFromD80 = 2,
    #[strum(serialize = "ALU", serialize = "DREG")] DReg = 5,
    #[strum(serialize = "IDRVSEL")] DriveSelReg = 6,
    #[strum(serialize = "MEMORY")]  Memory = 7,
    GPReg(u8),
}

#[derive(FromRepr, Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum DestRegister {
    #[strum(serialize = "DISKOUT")]  RWToDSU = 0,
    #[strum(serialize = "CLOCKOUT")] ClockToDSU = 1,
    #[strum(serialize = "DATAOUT")]  DataToD80 = 2,
    #[strum(serialize = "AREG")]     AReg = 3,
    #[strum(serialize = "BREG")]     BReg = 4,
    #[strum(serialize = "ODRVSEL")]  DriveSelReg = 5,
    #[strum(serialize = "MAR")]      MAR = 6, // Memory Address Register
    #[strum(serialize = "MEMORY")]   Memory = 7,
    /*#[strum(serialize = "")] */    HeadLoadRegister = 8,
    GPReg(u8),
}

const MOVE_SOURCE_LITERAL:      u16 = 0b0010_0000_0000_0000;
const MOVE_SOURCE_LITERAL_MASK: u16 = 0b0001_1111_1110_0000;
const MOVE_SOURCE_GP:           u16 = 0b0000_0010_0000_0000;
const MOVE_SOURCE_REG:          u16 = 0b0000_0001_1110_0000;
const MOVE_DEST_GP:             u16 = 0b0000_0000_0001_0000;
const MOVE_DEST_REG:            u16 = 0b0000_0000_0000_1111;

#[derive(FromRepr, Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum Condition {
    #[strum(serialize = "NOP0")]        NoOperation = 0,
    #[strum(serialize = "RDYREQ")]      ByteRdyRqst,

    #[strum(serialize = "FILL")]        FillBfrCmd = 3,
    #[strum(serialize = "EMPTY")]       EmptyBfrCmd,
    #[strum(serialize = "NOTREADY")]    Ready,
    #[strum(serialize = "HOLE")]        IndexHole,
    #[strum(serialize = "CLKDOWN")]     HeadTimedOut,          // "Timed Head Down (20MS)" in listing comments
    #[strum(serialize = "CRCERR")]      CRCError,
    #[strum(serialize = "D0SELF")]      Drive1Sel,
    #[strum(serialize = "")]            IDClockPattern,


    #[strum(serialize = "TIMER")]       TimerDone = 0o15,

    #[strum(serialize = "DOWN")]        SelHeadDown = 0o17,
    #[strum(serialize = "D5LOAD")]      D5Load,
    #[strum(serialize = "ACK")]         ACK,
    #[strum(serialize = "TERMINATE")]   Terminate,
    #[strum(serialize = "READ")]        ReadDataCmd,
    #[strum(serialize = "WRITE")]       WriteDataCmd,
    #[strum(serialize = "SEEK")]        SeekCmd,
    #[strum(serialize = "FORMAT")]      FrmtDiskCmd = 0o26,    // IF DF INHOUSE
    //#[strum(serialize = "READAFTW")]  ReadAfterWrite = 0o26, // Not inhouse??
    #[strum(serialize = "D1SELF")]      Drive2Sel,
    #[strum(serialize = "D2SELF")]      Drive3Sel,
    #[strum(serialize = "COMAND")]      CmdRdy,
    #[strum(serialize = "WRITEPROT")]   WriteProtect,
    #[strum(serialize = "WRITEDEL")]    WriteDataCmd2,
    #[strum(serialize = "TRACK00")]     Track00,
    #[strum(serialize = "C",
            serialize = "CS")]          ALUCarry,
    #[strum(serialize = "EQ")]          ALUEqual,
    #[strum(serialize = "DSSELF")]      Drive4Sel,

    //                                  Two Board
    #[strum(serialize = "MEMRDY")]      MemoryReady,           // 0o2
    #[strum(serialize = "SECTOR")]      SectorHeaderMark,      // 0o12
    #[strum(serialize = "DATAID")]      DataId,                // 0o13
    #[strum(serialize = "DELDATA")]     DelData,               // 0o14
    #[strum(serialize = "INOP")]        FileInop,              // 0o16
}

#[derive(FromRepr, Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum ALUMode {
    // Name in code listing               74LS181 manual (but converted to modern PL operators:

    /*                                     !C M S3-S0   -> Operation    */
    /*_ = 0b01_0000,*/NOTA = 0b11_0000,  /* _ 1 0 0 0 0 -> F = !A       */
    /*_ = 0b01_0001,*/NOR  = 0b11_0001,  /* _ 1 0 0 0 1 -> F = !(A | B) */
    /*_ = 0b01_0010,*//*_ = 0b11_0010,*/ /* _ 1 0 0 1 0 -> F = !A & B   */
    /*_ = 0b01_0011,*//*_ = 0b11_0011,*/ /* _ 1 0 0 1 1 -> F = 0        */
    /*_ = 0b01_0100,*/NAND = 0b11_0100,  /* _ 1 0 1 0 0 -> F = !(A & B) */
    /*_ = 0b01_0101,*/NOTB = 0b11_0101,  /* _ 1 0 1 0 1 -> F = !B       */
    /*_ = 0b01_0110,*/XOR  = 0b11_0110,  /* _ 1 0 1 1 0 -> F = A ^ B    */
    /*_ = 0b01_0111,*//*_ = 0b11_0111,*/ /* _ 1 0 1 1 1 -> F = (A & !B) */
    /*_ = 0b01_1000,*//*_ = 0b11_1000,*/ /* _ 1 1 0 0 0 -> F = !A | B   */
    /*_ = 0b01_1001,*/XORBAR= 0b11_1001, /* _ 1 1 0 0 1 -> F = !(A ^ B) */
    /*_ = 0b01_1010,*/CPYB = 0b11_1010,  /* _ 1 1 0 1 0 -> F = B        */
    /*_ = 0b01_1011,*/AND  = 0b11_1011,  /* _ 1 1 0 1 1 -> F = A & B    */
    /*_ = 0b01_1100,*//*_ = 0b11_1100,*/ /* _ 1 1 1 0 0 -> F = 1        */
    /*_ = 0b01_1101,*//*_ = 0b11_1101,*/ /* _ 1 1 1 0 1 -> F = A | !B   */
    /*_ = 0b01_1110,*/OR   = 0b11_1110,  /* _ 1 1 1 1 0 -> F = A | B    */
    /*_ = 0b01_1111,*/CPYA = 0b11_1111,  /* _ 1 1 1 1 1 -> F = A        */

    /*                                     !C M S3-S0   -> Operation              */
    /*_ = 0b10_0000,*/                   /* 1 0 0 0 0 0 -> F = A                  */
    /*_ = 0b10_0001,*/                   /* 1 0 0 0 0 1 -> F = A | B              */
    /*_ = 0b10_0010,*/                   /* 1 0 0 0 1 0 -> F = A | !B             */
    /*_ = 0b10_0011,*/                   /* 1 0 0 0 1 1 -> F = - 1 (2's COMPL)    */
    /*_ = 0b10_0100,*/                   /* 1 0 0 1 0 0 -> F = A + (A & !B)       */
    /*_ = 0b10_0101,*/                   /* 1 0 0 1 0 1 -> F = (A | B) + (A & !B) */
    CMP = 0b10_0110,                     /* 1 0 0 1 1 0 -> F = A - B - 1          */
    /*_ = 0b10_0111,*/                   /* 1 0 0 1 1 1 -> F = (A & !B) - 1       */
    /*_ = 0b10_1000,*/                   /* 1 0 1 0 0 0 -> F = A + (A & B)        */
    PLUS = 0b10_1001,                    /* 1 0 1 0 0 1 -> F = A + B              */
    /*_ = 0b10_1010,*/                   /* 1 0 1 0 1 0 -> F = (A | !B) + (A & B) */
    /*_ = 0b10_1011,*/                   /* 1 0 1 0 1 1 -> F = (A & B) - 1        */
    ROL = 0b10_1100,                     /* 1 0 1 1 0 0 -> F = A + A              */
    /*_ = 0b10_1101,*/                   /* 1 0 1 1 0 1 -> F = (A | B) + A        */
    /*_ = 0b10_1110,*/                   /* 1 0 1 1 1 0 -> F = (A | !B) + A       */
    DEC = 0b10_1111,                     /* 1 0 1 1 1 1 -> F = A - 1              */

    /*                                     !C M S3-S0   -> Operation                  */
    INC = 0b00_0000,                     /* 0 0 0 0 0 0 -> F = A + 1                  */
    /*_ = 0b00_0001,*/                   /* 0 0 0 0 0 1 -> F = (A | B) + 1            */
    /*_ = 0b00_0010,*/                   /* 0 0 0 0 1 0 -> F = (A | !B) + 1           */
    /*_ = 0b00_0011,*/                   /* 0 0 0 0 1 1 -> F = ZERO                   */
    /*_ = 0b00_0100,*/                   /* 0 0 0 1 0 0 -> F = A + (A & !B) + 1       */
    /*_ = 0b00_0101,*/                   /* 0 0 0 1 0 1 -> F = (A | B) + (A & !B) + 1 */
    MINUS = 0b00_0110,                   /* 0 0 0 1 1 0 -> F = A - B                  */
    /*_ = 0b00_0111,*/                   /* 0 0 0 1 1 1 -> F = (A & !B)               */
    /*_ = 0b00_1000,*/                   /* 0 0 1 0 0 0 -> F = A + (A & B) + 1        */
    /*_ = 0b00_1001,*/                   /* 0 0 1 0 0 1 -> F = A + B + 1              */
    /*_ = 0b00_1010,*/                   /* 0 0 1 0 1 0 -> F = (A | !B) + (A & B) + 1 */
    /*_ = 0b00_1011,*/                   /* 0 0 1 0 1 1 -> F = A & B                  */
    /*_ = 0b00_1100,*/                   /* 0 0 1 1 0 0 -> F = A + A + 1              */
    /*_ = 0b00_1101,*/                   /* 0 0 1 1 0 1 -> F = (A | B) + A + 1        */
    /*_ = 0b00_1110,*/                   /* 0 0 1 1 1 0 -> F = (A | !B) + A + 1       */
    /*_ = 0b00_1111,*/                   /* 0 0 1 1 1 1 -> F = A                      */
}

#[derive(FromRepr, Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum Function {
    HEADIN       = 0o00, // SET THE HEAD DIRECTION TO IN
    HEADOUT      = 0o01, // SET THE HEAD DIRECTION TO OUT
    STEP         = 0o02, // STEP THE HEAD IN HTE FROPER DIRECTION
    WRITEGATE    = 0o03, // SET THE WRITE GATE
    ENAMRITE     = 0o04, // ENABLE EHT WRITE LOGIC
    ENACRC       = 0o05, // ENABLE CRO LOGIC
    DISCRC       = 0o06, // DISKABLE THE CRC LOGIC
    ENASHIFT     = 0o07, // ENABLE SHIFT CRC
    DISSHIFT     = 0o10, // DISABLE SHIFT CRC
    LOAD         = 0o11, // LOAD THE HEAD
    UNLOAD       = 0o12, // UNLOAD THE HEAD
    SETDEVCHECK  = 0o13, // SET DEVICE CHECK ERROR
    SETDATACRC   = 0o14, // SET THE DATA CRC ERROR FLOF
    SETSEEK      = 0o15, // SET SEEK ERROR FLOP
    SETWRITEPROT = 0o16, // SET WRITE PROTECT ERROR FLOP
    SETNOTRDY    = 0o17, // SET SELECTED DRIVE NOT READY
    CMDDONE      = 0o20, // COMMAND DONE PULSE
    SETDELDATA   = 0o21, // SET THE DELETED DATA READ FLAG
    CLEARSTATUS  = 0o22, // CLEAR THE STATUS REGISTER
    SETCONRDY    = 0o23, // SET CONTROLER READY
    RESETINOP    = 0o24, // REST FILE INOPERABLE
    SETCOMBINED  = 0o25, // SET THE COMBINED ERROR BIT IN THE INTERFACE
    CLEARCMD     = 0o26, // CLEAR THE COMMAND REGISTER
    NOP          = 0o27,
    RESETWRITE   = 0o30, // RESET WRITE ENABLE/GATE
    INCMAR       = 0o31, // INCREMENT THE MEMORY ADDRESS REGISTER
    CDFO         = 0o32, // S E T TO JUMP TO ROM F I E L D 0 0
    CDF1         = 0o33, // S E T TO J U M P TO ROM F I E L D Oi
    SETABV43     = 0o34, // ABOVE TRACK 43 TO DISK DRIVES
    SETBELW43    = 0o35, // BELOW TRACK 43,
    ALSTATUS     = 0o36, // SEND STATS ON COMMAND COMPLETION
}

pub trait DecodeInstruction {
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>>;
}

pub struct TwoBoard {
}

impl DecodeInstruction for TwoBoard {
    fn decode(&self, word: u16) -> Result<Instruction, Box<dyn Error>> {
        Ok(match (word & 0b1100_0000_0000_0000) >> 14 {
            0b00 => self.decode_jump(word)?,
            0b01 => self.decode_funct_timer(word)?,
            0b10 => self.decode_funct_alu(word)?,
            0b11 => self.decode_move(word)?,
            _ => unreachable!(),
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
        Ok(match (word & 0b1100_0000_0000_0000) >> 14 {
            0b00 => self.decode_jump(true, word)?,
            0b01 => self.decode_jump(false, word)?,
            0b10 => self.decode_funct_alu_timer(word)?,
            0b11 => self.decode_move(word)?,
            _ => unreachable!(),
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
