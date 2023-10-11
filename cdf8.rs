// Copyright © 2023 David Caldwell <david@porkrind.org>

use strum_macros::{Display,FromRepr};

pub const OPCODE_MASK: u16 = 0b1100_0000_0000_0000;

#[derive(Debug, PartialEq, Clone, FromRepr)]
#[repr(u16)]
pub enum RawOpcodeOneBoard {
    JumpTrue         = 0b0000_0000_0000_0000,
    JumpFalse        = 0b0100_0000_0000_0000,
    FunctionAluTimer = 0b1000_0000_0000_0000,
    Move             = 0b1100_0000_0000_0000,
}

#[derive(Debug, PartialEq, Clone, FromRepr)]
#[repr(u16)]
pub enum RawOpcodeTwoBoard {
    Jump          = 0b0000_0000_0000_0000,
    FunctionTimer = 0b0100_0000_0000_0000,
    FunctionAlu   = 0b1000_0000_0000_0000,
    Move          = 0b1100_0000_0000_0000,
}

#[derive(Debug,Clone)]
pub enum Instruction {
    Jump{ when: bool, condition: Condition, effective_address: u16 },
    FunctionTimer { timer: Timer, function: Function },
    FunctionALU { alu: Option<ALU>, function: Function },
    Move{ source: MoveSource, dest: DestRegister },
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

#[derive(Debug,Clone)]
pub struct Timer {
    pub negative_count: u8,
    pub clock_rate: ClockRate,
}

#[derive(Debug, Clone, Display)]
pub enum ClockRate {
    #[strum(serialize = "MS")] Millisecond,
    #[strum(serialize = "BT")] Microsecond, // BT???
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

#[derive(Debug, PartialEq, Clone, FromRepr, Display)]
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

#[derive(FromRepr, Debug, Display, PartialEq, Clone)]
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

#[derive(FromRepr, Display, Debug, PartialEq, Clone)]
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

#[derive(FromRepr, Debug, Display, PartialEq, Clone)]
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

#[derive(FromRepr, Debug, Display, PartialEq, Clone)]
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

// This was a thing in the two board version (replaced by real immediates in the one board mv instruction
pub const CONSTANT_ROM: [u8;32] = [
    0xff,  /* 0o0  */ 0xdf,  /* 0o1  */ 0xfc,  /* 0o2  */ 0xc7,  /* 0o3  */ 0xfe,  /* 0o4  */ 0xfb,  /* 0o5  */ 0x20,  /* 0o6  */ 0x11,  /* 0o7  */
    0x2e,  /* 0o10 */ 0x80,  /* 0o11 */ 0x4c,  /* 0o12 */ 0xfd,  /* 0o13 */ 0x1e,  /* 0o14 */ 0x00,  /* 0o15 */ 0x21,  /* 0o16 */ 0xf8,  /* 0o17 */
    0xe5,  /* 0o20 */ 0xf5,  /* 0o21 */ 0xfa,  /* 0o22 */ 0x01,  /* 0o23 */ 0x99,  /* 0o24 */ 0x02,  /* 0o25 */ 0x03,  /* 0o26 */ 0x04,  /* 0o27 */
    0x1a,  /* 0o30 */ 0x7c,  /* 0o31 */ 0x4f,  /* 0o32 */ 0x2b,  /* 0o33 */ 0xfc,  /* 0o34 */ 0x1b,  /* 0o35 */ 0x1c,  /* 0o36 */ 0 // unused 0o37
];

