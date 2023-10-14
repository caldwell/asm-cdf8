// Copyright © 2023 David Caldwell <david@porkrind.org>

use strum_macros::{Display,FromRepr, EnumString, EnumMessage, EnumIter};

pub const OPCODE_MASK: u16 = 0b1100_0000_0000_0000;

#[derive(Debug, PartialEq, Clone, FromRepr)]
#[repr(u16)]
pub enum RawOpcodeOneBoard {
    JumpTrue         = 0b0000_0000_0000_0000,
    JumpFalse        = 0b0100_0000_0000_0000,
    FunctionALUTimer = 0b1000_0000_0000_0000,
    Move             = 0b1100_0000_0000_0000,
}

#[derive(Debug, PartialEq, Clone, FromRepr)]
#[repr(u16)]
pub enum RawOpcodeTwoBoard {
    Jump          = 0b0000_0000_0000_0000,
    FunctionTimer = 0b0100_0000_0000_0000,
    FunctionALU   = 0b1000_0000_0000_0000,
    Move          = 0b1100_0000_0000_0000,
}

#[derive(Debug,Clone, PartialEq, Eq)]
pub enum Instruction {
    Jump{ when: bool, condition: Condition, effective_address: JumpDest },
    FunctionTimer { timer: Timer, function: Function },
    FunctionALU { alu: Option<ALUMode>, function: Function },
    Move{ source: MoveSource, dest: DestRegister },
}

#[derive(Debug,Clone, PartialEq, Eq)]
pub enum JumpDest {
    Absolute(u16),
    Symbol(String),
    Myself, // For WAIT instruction
}

impl std::fmt::Display for JumpDest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(match self {
            JumpDest::Absolute(a) => write!(f, "{:o}", a)?,
            JumpDest::Symbol(s) => write!(f, "{}", s.clone())?,
            JumpDest::Myself => write!(f, "Myself")?,
        })
    }
}

pub const TIMER_ALU_SELECT:       u16 = 0b0000_0001_0000_0000;
pub const TIMER_ALU_SELECT_TIMER: u16 = 0b0000_0001_0000_0000;
//pub const TIMER_ALU_SELECT_ALU:   u16 = 0b0000_0000_0000_0000;
pub const ALU_FUNCT_SELECT:       u16 = 0b0000_0000_1000_0000;
pub const ALU_FUNCT_SELECT_ALU:   u16 = 0b0000_0000_1000_0000;
//pub const ALU_FUNCT_SELECT_FUNCT: u16 = 0b0000_0000_0000_0000;

pub const MOVE_SOURCE_LITERAL:      u16 = 0b0010_0000_0000_0000;
pub const MOVE_SOURCE_LITERAL_MASK: u16 = 0b0001_1111_1110_0000;
pub const MOVE_SOURCE_GP:           u16 = 0b0000_0010_0000_0000;
pub const MOVE_SOURCE_REG:          u16 = 0b0000_0001_1110_0000;
pub const MOVE_DEST_GP:             u16 = 0b0000_0000_0001_0000;
pub const MOVE_DEST_REG:            u16 = 0b0000_0000_0000_1111;

#[derive(Debug,Clone, PartialEq, Eq)]
pub struct Timer {
    pub negative_count: u8,
    pub clock_rate: ClockRate,
}

#[derive(Debug, Clone, Display, PartialEq, Eq)]
pub enum ClockRate {
    #[strum(serialize = "MS")] Millisecond,
    #[strum(serialize = "BT")] Microsecond, // BT???
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MoveSource {
    Literal(u8),
    Register(SourceRegister),
    Constant(u8),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromRepr, EnumString, EnumIter, Display)]
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

pub trait ToU8 {
    fn as_u8(&self) -> u8 {
        // This (crazily) is how the rust Reference says to get the discriminant (can't do plain `as u8` because of SourceRegister::GPReg)
        // https://doc.rust-lang.org/reference/items/enumerations.html#pointer-casting
        unsafe { *(*&self as *const Self as *const u8) }
    }
}
impl ToU8 for SourceRegister {}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromRepr, EnumString, EnumIter, Display)]
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
    #[strum(serialize = "HEADLOAD")] HeadLoadRegister = 8, // I made up the serialized name because it didn't exist in the TwoBoard listing.
    GPReg(u8),
}

impl ToU8 for DestRegister {}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromRepr, EnumString, EnumMessage, EnumIter, Display)]
#[repr(u8)]
pub enum Condition {
    /** 0, 0 */  #[strum(serialize = "NOP0")]        NoOperation         = 0o00,
    /** 0, 1 */  #[strum(serialize = "RDYREQ")]      ByteRdyRqst         = 0o01,
    /*  0, 2 */  // Unused                                               = 0o02,
    /** 0, 3 */  #[strum(serialize = "FILL")]        FillBfrCmd          = 0o03,
    /** 0, 4 */  #[strum(serialize = "EMPTY")]       EmptyBfrCmd         = 0o04,
    /** 0, 5 */  #[strum(serialize = "NOTREADY")]    NotReady            = 0o05,
    /** 0, 6 */  #[strum(serialize = "HOLE")]        IndexHole           = 0o06,
    /** 0, 7 "Timed Head Down (20MS)" in listing comments  */
                 #[strum(serialize = "CLKDOWN")]     HeadTimedOut        = 0o07,
    /** 0, 8 */  #[strum(serialize = "CRCERR")]      CRCError            = 0o10,
    /** 0, 9 */  #[strum(serialize = "D0SELF")]      Drive1SelBar        = 0o11,
    /** 0,10 */  #[strum(serialize = "IDCLKPAT")]    IDClockPattern      = 0o12,
    /*  0,11 */  // Unused                                               = 0o13,
    /*  0,12 */  // Unused                                               = 0o14,
    /** 0,13 */  #[strum(serialize = "TIMER")]       TimerDone           = 0o15,
    /*  0,14 */  // Unused                                               = 0o16,
    /** 0,15 */  #[strum(serialize = "DOWN")]        SelHeadDown         = 0o17,
    /** 1, 0 */  #[strum(serialize = "D5LOAD")]      D5Load              = 0o20,
    /** 1, 1 */  #[strum(serialize = "ACK")]         ACK                 = 0o21,
    /** 1, 2 */  #[strum(serialize = "TERMINATE")]   TerminateBar        = 0o22,
    /** 1, 3 */  #[strum(serialize = "READ")]        ReadDataCmd         = 0o23,
    /** 1, 4 */  #[strum(serialize = "WRITE")]       WriteDataCmd        = 0o24,
    /** 1, 5 */  #[strum(serialize = "SEEK")]        SeekCmd             = 0o25,
    /** 1, 6 ".IF DF INHOUSE" in original listing.  */
                 #[strum(serialize = "FORMAT")]      FrmtDiskCmd         = 0o26,
    /* 1, 6 Not inhouse??  */
    //           #[strum(serialize = "READAFTW")]    ReadAfterWrite      = 0o26,
    /** 1, 7 */  #[strum(serialize = "D1SELF")]      Drive2SelBar        = 0o27,
    /** 1, 8 */  #[strum(serialize = "D2SELF")]      Drive3SelBar        = 0o30,
    /** 1, 9 */  #[strum(serialize = "COMMAND")]     CmdRdy              = 0o31,
    /** 1,10 */  #[strum(serialize = "WRITEPROT")]   WriteProtect        = 0o32,
    /** 1,11 */  #[strum(serialize = "WRITEDEL")]    WriteDeletedDataCmd = 0o33,
    /** 1,12 */  #[strum(serialize = "TRACK00")]     Track00             = 0o34,
    /** 1,13 */  #[strum(serialize = "C",
                         serialize = "CS")]          ALUCarry            = 0o35,
    /** 1,14 */  #[strum(serialize = "EQ")]          ALUEqual            = 0o36,
    /** 1,15 */  #[strum(serialize = "DSSELF")]      Drive4Sel           = 0o37,

    // Two Board Only
    /** 0, 2 */  #[strum(serialize = "MEMRDY")]      MemoryReady         = 0o40+0o2,
    /** 0,10 */  #[strum(serialize = "SECTOR")]      SectorHeaderMark    = 0o40+0o12,
    /** 0,11 */  #[strum(serialize = "DATAID")]      DataId              = 0o40+0o13,
    /** 0,12 */  #[strum(serialize = "DELDATA")]     DelData             = 0o40+0o14,
    /** 0,14 */  #[strum(serialize = "INOP")]        FileInop            = 0o40+0o16,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromRepr, EnumString, EnumMessage, EnumIter, Display)]
#[repr(u8)]
pub enum ALUMode {
    // Name in code listing               74LS181 manual (but converted to modern PL operators:

    /** !C M S3-S0   -> Operation (Logical) */
    /**  _ 1 0 0 0 0 -> F = !A       */                /*_ = 0b01_0000,*/NOTA   = 0b11_0000,
    /**  _ 1 0 0 0 1 -> F = !(A | B) */                /*_ = 0b01_0001,*/NOR    = 0b11_0001,
    /**  _ 1 0 0 1 0 -> F = !A & B   */                /*_ = 0b01_0010,*//*_    = 0b11_0010,*/
    /**  _ 1 0 0 1 1 -> F = 0        */                /*_ = 0b01_0011,*//*_    = 0b11_0011,*/
    /**  _ 1 0 1 0 0 -> F = !(A & B) */                /*_ = 0b01_0100,*/NAND   = 0b11_0100,
    /**  _ 1 0 1 0 1 -> F = !B       */                /*_ = 0b01_0101,*/NOTB   = 0b11_0101,
    /**  _ 1 0 1 1 0 -> F = A ^ B    */                /*_ = 0b01_0110,*/XOR    = 0b11_0110,
    /**  _ 1 0 1 1 1 -> F = (A & !B) */                /*_ = 0b01_0111,*//*_    = 0b11_0111,*/
    /**  _ 1 1 0 0 0 -> F = !A | B   */                /*_ = 0b01_1000,*//*_    = 0b11_1000,*/
    /**  _ 1 1 0 0 1 -> F = !(A ^ B) */                /*_ = 0b01_1001,*/XORBAR = 0b11_1001,
    /**  _ 1 1 0 1 0 -> F = B        */                /*_ = 0b01_1010,*/CPYB   = 0b11_1010,
    /**  _ 1 1 0 1 1 -> F = A & B    */                /*_ = 0b01_1011,*/AND    = 0b11_1011,
    /**  _ 1 1 1 0 0 -> F = 1        */                /*_ = 0b01_1100,*//*_    = 0b11_1100,*/
    /**  _ 1 1 1 0 1 -> F = A | !B   */                /*_ = 0b01_1101,*//*_    = 0b11_1101,*/
    /**  _ 1 1 1 1 0 -> F = A | B    */                /*_ = 0b01_1110,*/OR     = 0b11_1110,
    /**  _ 1 1 1 1 1 -> F = A        */                /*_ = 0b01_1111,*/CPYA   = 0b11_1111,

    /** !C M S3-S0   -> Operation (Arithmetic, No Carry) */
    /**  1 0 0 0 0 0 -> F = A                  */      /*_ = 0b10_0000,*/
    /**  1 0 0 0 0 1 -> F = A | B              */      /*_ = 0b10_0001,*/
    /**  1 0 0 0 1 0 -> F = A | !B             */      /*_ = 0b10_0010,*/
    /**  1 0 0 0 1 1 -> F = - 1 (2's COMPL)    */      /*_ = 0b10_0011,*/
    /**  1 0 0 1 0 0 -> F = A + (A & !B)       */      /*_ = 0b10_0100,*/
    /**  1 0 0 1 0 1 -> F = (A | B) + (A & !B) */      /*_ = 0b10_0101,*/
    /**  1 0 0 1 1 0 -> F = A - B - 1          */      CMP = 0b10_0110,
    /**  1 0 0 1 1 1 -> F = (A & !B) - 1       */      /*_ = 0b10_0111,*/
    /**  1 0 1 0 0 0 -> F = A + (A & B)        */      /*_ = 0b10_1000,*/
    /**  1 0 1 0 0 1 -> F = A + B              */      PLUS = 0b10_1001,
    /**  1 0 1 0 1 0 -> F = (A | !B) + (A & B) */      /*_ = 0b10_1010,*/
    /**  1 0 1 0 1 1 -> F = (A & B) - 1        */      /*_ = 0b10_1011,*/
    /**  1 0 1 1 0 0 -> F = A + A              */      ROL = 0b10_1100,
    /**  1 0 1 1 0 1 -> F = (A | B) + A        */      /*_ = 0b10_1101,*/
    /**  1 0 1 1 1 0 -> F = (A | !B) + A       */      /*_ = 0b10_1110,*/
    /**  1 0 1 1 1 1 -> F = A - 1              */      DEC = 0b10_1111,

    /** !C M S3-S0   -> Operation (Arithmetic, Carry) */
    /**  0 0 0 0 0 0 -> F = A + 1                  */  INC = 0b00_0000,
    /**  0 0 0 0 0 1 -> F = (A | B) + 1            */  /*_ = 0b00_0001,*/
    /**  0 0 0 0 1 0 -> F = (A | !B) + 1           */  /*_ = 0b00_0010,*/
    /**  0 0 0 0 1 1 -> F = ZERO                   */  /*_ = 0b00_0011,*/
    /**  0 0 0 1 0 0 -> F = A + (A & !B) + 1       */  /*_ = 0b00_0100,*/
    /**  0 0 0 1 0 1 -> F = (A | B) + (A & !B) + 1 */  /*_ = 0b00_0101,*/
    /**  0 0 0 1 1 0 -> F = A - B                  */  MINUS = 0b00_0110,
    /**  0 0 0 1 1 1 -> F = (A & !B)               */  /*_ = 0b00_0111,*/
    /**  0 0 1 0 0 0 -> F = A + (A & B) + 1        */  /*_ = 0b00_1000,*/
    /**  0 0 1 0 0 1 -> F = A + B + 1              */  /*_ = 0b00_1001,*/
    /**  0 0 1 0 1 0 -> F = (A | !B) + (A & B) + 1 */  /*_ = 0b00_1010,*/
    /**  0 0 1 0 1 1 -> F = A & B                  */  /*_ = 0b00_1011,*/
    /**  0 0 1 1 0 0 -> F = A + A + 1              */  /*_ = 0b00_1100,*/
    /**  0 0 1 1 0 1 -> F = (A | B) + A + 1        */  /*_ = 0b00_1101,*/
    /**  0 0 1 1 1 0 -> F = (A | !B) + A + 1       */  /*_ = 0b00_1110,*/
    /**  0 0 1 1 1 1 -> F = A                      */  /*_ = 0b00_1111,*/
    #[strum(serialize=";;")] __, // Hack :-) This has to be here to get the docs bound to it. The
                                 // serialization (a) makes it look good in the --details dump, and (b) makes
                                 // it so it can't serialize back into something when assembling
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromRepr, EnumString, EnumMessage, EnumIter, Display)]
#[repr(u8)]
pub enum Function {
    /** 0, 0 SET THE HEAD DIRECTION TO IN                */    HEADIN       = 0o00,
    /** 0, 1 SET THE HEAD DIRECTION TO OUT               */    HEADOUT      = 0o01,
    /** 0, 2 STEP THE HEAD IN THE PROPER DIRECTION       */    STEP         = 0o02,
    /** 0, 3 SET THE WRITE GATE                          */    WRITEGATE    = 0o03,
    /** 0, 4 ENABLE THE WRITE LOGIC                      */    ENAWRITE     = 0o04,
    /** 0, 5 ENABLE CRC LOGIC                            */    ENACRC       = 0o05,
    /** 0, 6 DISKABLE THE CRC LOGIC                      */    DISCRC       = 0o06,
    /** 0, 7 ENABLE SHIFT CRC                            */    ENASHIFT     = 0o07,
    /** 0, 8 DISABLE SHIFT CRC                           */    DISSHIFT     = 0o10,
    /*  0, 9                                             */    // Unused
    /** 0,10 SET SEEK ERROR FLOP                         */    SETSEEK      = 0o12,
    /** 0,11 SET DEVICE CHECK ERROR                      */    SETDEVCHECK  = 0o13,
    /** 0,12 SET THE DATA CRC ERROR FLOP                 */    SETDATACRC   = 0o14,
    /** 0,13 CLEAR TIMER REGISTER                        */    CLEARTIMER   = 0o15,
    /** 0,14 SET WRITE PROTECT ERROR FLOP                */    SETWRITEPROT = 0o16,
    /** 0,15 SET SELECTED DRIVE NOT READY                */    SETNOTRDY    = 0o17,
    /** 1, 0 COMMAND DONE PULSE                          */    CMDDONE      = 0o20,
    /** 1, 1 SET THE DELETED DATA READ FLAG              */    SETDELDATA   = 0o21,
    /** 1, 2 CLEAR THE STATUS REGISTER                   */    CLEARSTATUS  = 0o22,
    /** 1, 3 SET CONTROLER READY                         */    SETCONRDY    = 0o23,
    /*  1, 4                                             */    // Unused
    /** 1, 5 SET THE COMBINED ERROR BIT IN THE INTERFACE */    SETCOMBINED  = 0o25,
    /** 1, 6 CLEAR THE COMMAND REGISTER                  */    CLEARCMD     = 0o26,
    /** 1, 7 Unused                                      */    NOP          = 0o27,
    /** 1, 8 RESET WRITE ENABLE/GATE                     */    RESETWRITE   = 0o30,
    /** 1, 9 INCREMENT THE MEMORY ADDRESS REGISTER       */    INCMAR       = 0o31,
    /** 1,10 Set Status Present flop                     */    SETSTPRES    = 0o32,
    /** 1,11 Set Diagnostic ok LED                       */    SETDIAGOK    = 0o33,
    /** 1,12 Set Soft CRC Error LED                      */    SETSOFTCRC   = 0o34,
    /** 1,13 Set Drive Check LED                         */    SETDRIVECK   = 0o35,
    /*  1,14                                             */    // Unused
    /*  1,15                                             */    // Unused

    // TwoBoard (offset by 0o40 (0x20)
    /** 0, 9 LOAD THE HEAD                               */    LOAD         = 0o40+0o11,
    /** 0,10 UNLOAD THE HEAD                             */    UNLOAD       = 0o40+0o12,
    /** 0,13 SET SEEK ERROR FLOP                         */    SETSEEK2     = 0o40+0o15,
    /** 1, 4 REST FILE INOPERABLE                        */    RESETINOP    = 0o40+0o24,
    /** 1,10 SET TO JUMP TO ROM FIELD00                  */    CDF0         = 0o40+0o32,
    /** 1,11 SET TO JUMP TO ROM FIELD01                  */    CDF1         = 0o40+0o33,
    /** 1,12 ABOVE TRACK 43 TO DISK DRIVES               */    SETABV43     = 0o40+0o34,
    /** 1,13 BELOW TRACK 43,                             */    SETBELW43    = 0o40+0o35,
    /** 1,14 SEND STATS ON COMMAND COMPLETION            */    ALSTATUS     = 0o40+0o36,
}

// This was a thing in the two board version (replaced by real immediates in the one board mv instruction)
pub const CONSTANT_ROM: [u8;32] = [
    0xff,  /* 0o0  */ 0xd7,  /* 0o1  */ 0xfc,  /* 0o2  */ 0xc7,  /* 0o3  */ 0xfe,  /* 0o4  */ 0xfb,  /* 0o5  */ 0x20,  /* 0o6  */ 0x11,  /* 0o7  */
    0x2e,  /* 0o10 */ 0x80,  /* 0o11 */ 0x4c,  /* 0o12 */ 0xfd,  /* 0o13 */ 0x1e,  /* 0o14 */ 0x00,  /* 0o15 */ 0x21,  /* 0o16 */ 0xf8,  /* 0o17 */
    0xe5,  /* 0o20 */ 0xf5,  /* 0o21 */ 0xfa,  /* 0o22 */ 0x01,  /* 0o23 */ 0x99,  /* 0o24 */ 0x02,  /* 0o25 */ 0x03,  /* 0o26 */ 0x04,  /* 0o27 */
    0x1a,  /* 0o30 */ 0x7c,  /* 0o31 */ 0x4f,  /* 0o32 */ 0x2b,  /* 0o33 */ 0xfc,  /* 0o34 */ 0x1b,  /* 0o35 */ 0x1c,  /* 0o36 */ 0xff // unused 0o37
];

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn constant_rom() {
        // An alternate dump of the constant rom to double check my "transcribed from faded dot-matrix listing printout" array.
        let dump: [u8; 32] = [
            0b11111111,
            0b11010111,
            0b11111100,
            0b11000111,
            0b11111110,
            0b11111011,
            0b00100000,
            0b00010001,
            0b00101110,
            0b10000000,
            0b01001100,
            0b11111101,
            0b00011110,
            0b00000000,
            0b00100001,
            0b11111000,
            0b11100101,
            0b11110101,
            0b11111010,
            0b00000001,
            0b10011001,
            0b00000010,
            0b00000011,
            0b00000100,
            0b00011010,
            0b01111100,
            0b01001111,
            0b00101011,
            0b11111100,
            0b00011011,
            0b00011100,
            0b11111111,
        ];
        assert_eq!(CONSTANT_ROM, dump);
    }
}
