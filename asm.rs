// Copyright © 2023 David Caldwell <david@porkrind.org>

use std::{error::Error, io::{Read, BufReader, BufRead}, str::FromStr};

use crate::{cdf8::*, WithContext};
use crate::dis::SymbolTable; // FIXME

pub fn assemble<R: ?Sized>(input: &R) -> Result<Vec<u16>, Box<dyn Error>>
where for<'a> &'a R: Read
{
    let reader = BufReader::new(input);

    let (program, symbols) = parse(reader.lines().map(|rl| rl.expect("io error")))?;

    let words = emit(program, symbols)?;

    Ok(words)
}

fn parse<I>(lines: I) -> Result<(Vec<Instruction>, SymbolTable), Box<dyn Error>>
where
    I: IntoIterator<Item = String>,
{
    let mut program: Vec<Instruction> = Vec::new();
    let mut symbols = SymbolTable::new();

    let mut addr = 0;
    for (line_num, line) in lines.into_iter().enumerate() {
        match line.parse::<Line>() {
            Ok(Line::BlankLine) => {},
            Ok(Line::LabeledInstruction(LabeledInstruction { label, insn })) => {
                if let Some(label) = label {
                    symbols.insert(label, addr);
                }
                program.push(insn);
                addr += 1;
            },
            Ok(Line::SymbolDeclaration(SymbolDeclaration { name, value })) => {
                if let Some(current_value) = symbols.addr_for_symbol(&name) {
                    Err(format!("{}: Redefined symbol '{}'. Previous value was '{:#o}'", line_num, name, current_value))?;
                }
                symbols.insert(name, value);
            },
            Err(e) => {
                println!("{}: {} in\n{}", line_num, e, line);
            },
        }
    }
    Ok((program, symbols))
}


fn emit(program: Vec<Instruction>, symbols:SymbolTable) -> Result<Vec<u16>, Box<dyn Error>> {
    let words: Result<Vec<u16>,Box<dyn Error>> = program.into_iter().enumerate().map(|(addr, insn)| -> Result<u16, Box<dyn Error>> {
        let addr = addr as u16;
        Ok(match insn {
            Instruction::Jump { when, condition, effective_address } => {
                0 | if when { RawOpcodeOneBoard::JumpTrue as u16 }
                    else { RawOpcodeOneBoard::JumpFalse as u16 }
                  | 0b00_11111_0_0000_0000 & ((condition as u16) << 9)
                  | 0b00_00000_1_1111_1111 & match effective_address {
                      JumpDest::Symbol(name) => symbols.addr_for_symbol(&name).ok_or_else(|| format!("No label found for jump destination {}", name))?,
                      JumpDest::Absolute(address) => address,
                      JumpDest::Myself => addr,
                  }
            },
            Instruction::FunctionALU { alu, function } => {
                0 | RawOpcodeOneBoard::FunctionALUTimer as u16
                  | 0b00_11111_0_0000_0000 & (function as u16) << 9
                  | 0b00_00000_0_1011_1111 & match alu {
                      Some(alu) => ALU_FUNCT_SELECT_ALU | (alu as u16),
                      None => 0
                  }
            },
            Instruction::FunctionTimer { timer, function } => {
                0 | RawOpcodeOneBoard::FunctionALUTimer as u16
                  | 0b00_11111_0_0000_0000 & (function as u16) << 9
                  | 0b00_00000_1_1111_1111 & (TIMER_ALU_SELECT_TIMER | timer.negative_count as u16)
            },
            Instruction::Move { source, dest } => {
                0 | RawOpcodeOneBoard::Move as u16
                  | 0b00_11_1111_111_0_0000 & match source {
                      MoveSource::Literal(byte) => MOVE_SOURCE_LITERAL | (byte as u16) << 5,
                      MoveSource::Register(SourceRegister::GPReg(r)) => MOVE_SOURCE_GP | (r as u16) << 5,
                      MoveSource::Register(r) => ((unsafe { *(&r as *const SourceRegister as *const u8) }) as u16) << 5,
                      MoveSource::Constant(_) => Err(format!("The OneBoard hardware has no constant rom"))?,
                  }
                  | 0b00_00_00000_000_1_1111 & match dest {
                      DestRegister::GPReg(r) => MOVE_DEST_GP | (r as u16),
                      r => (unsafe { *(&r as *const DestRegister as *const u8) }) as u16,
                  }
            },
        })
    }).collect();

    words
}


/// Not really an assert since we don't panic, but what else to name it???
fn assert_blank(s: &str) -> Result<(), Box<dyn Error>> {
    let s = s.trim_start();
    if !s.starts_with(';') && s.len() != 0 {
        Err(format!("Extra cruft at end of line: {}", s))?
    }
    Ok(())
}

impl FromStr for Instruction {
    type Err=Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mnemonic, rest) = s.split_once(' ').unwrap_or((s, ""));
        let rest = rest.trim_start();

        let (insn, rest) = match mnemonic {
            "JTC" |
            "JFC" => {
                let (dest_s, cond_s) = rest.split_once(',').ok_or_else(|| format!("Missing comma in {} operands", mnemonic))?;

                (Instruction::Jump { when: mnemonic == "JTC", condition: cond_s.parse::<Condition>()?, effective_address: JumpDest::Symbol(dest_s.to_owned()) }, "")
            },
            "JUMP" => {(Instruction::Jump { when: false, condition: Condition::NoOperation, effective_address: JumpDest::Symbol(rest.to_owned()) }, "")},
            "NOP"  => {(Instruction::Jump { when: true,  condition: Condition::NoOperation, effective_address: JumpDest::Absolute(0) }, rest)},
            "WAIT" => {(Instruction::Jump { when: false, condition: rest.parse::<Condition>()?, effective_address: JumpDest::Myself }, "")},
            "F" => {
                (match rest.split_once(',').unwrap_or((rest, "")) {
                    ("", alu_s) => Instruction::FunctionALU { function: Function::NOP, alu: Some(alu_s.parse::<ALUMode>()?) },
                    (func_s, "") => Instruction::FunctionALU { function: func_s.parse::<Function>().with_context(format!("F {}", func_s))?, alu: None },
                    (func_s, alu_s) => Instruction::FunctionALU { function: func_s.parse::<Function>().with_context(format!("F,A {}", func_s))?, alu: Some(alu_s.parse::<ALUMode>()?) },
                }, "")
            },
            "CF" => {
                (match rest.split_once(',').ok_or_else(|| format!("Missing comma in CF operands"))? {
                    ("", timer_s) => Instruction::FunctionTimer { function: Function::NOP, timer: timer_s.parse::<Timer>()? },
                    (func_s, timer_s) => Instruction::FunctionTimer { function: func_s.parse::<Function>().with_context(format!("Bad Function '{}'", func_s))?,
                                                                      timer: timer_s.parse::<Timer>().with_context(format!("Bad Timer '{}'", timer_s))? },
                }, "")
            },
            "MV" => {
                let (src_s, dest_s) = rest.split_once(',').ok_or_else(|| format!("Missing comma in MV operands"))?;
                let source = src_s.trim_start().parse::<MoveSource>().with_context(format!("Move Source Register '{}'", src_s))?;
                let dest = if dest_s.starts_with("GP") {
                    DestRegister::GPReg(u8::from_str_radix(&dest_s[2..], 10)?)
                } else {
                    dest_s.trim_start().parse::<DestRegister>().with_context(format!("Move Dest Register '{}'", dest_s))?
                };
                (Instruction::Move { source, dest }, "")
            },
            _ => {
                Err(format!("Unknown instruction {}", mnemonic))?
            }
        };

        assert_blank(rest)?;

        Ok(insn)
    }
}


impl FromStr for MoveSource {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(if s.starts_with('#') {
            MoveSource::Literal(u8::from_str_radix(&s[1..], 8)?)
        } else if s.starts_with("GP") {
            MoveSource::Register(SourceRegister::GPReg(u8::from_str_radix(&s[2..], 10)?))
        } else {
            MoveSource::Register(s.parse::<SourceRegister>()?)
        })
    }
}

impl FromStr for Timer {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some(ms) = s.strip_suffix("MS") else {
            Err(format!("Timer can only have MS precision: {}", s))?
        };
        Ok(Timer { negative_count: (256u16 - u8::from_str_radix(&ms, 8)? as u16) as u8,
                   clock_rate: ClockRate::Microsecond })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Line {
    LabeledInstruction(LabeledInstruction),
    SymbolDeclaration(SymbolDeclaration),
    BlankLine, // or comment
}

impl FromStr for Line {
    type Err = String;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let line = match line.split_once(';') {
            Some((line, _comment)) => line,
            None => line,
        };

        let line = line.trim();

        if line.len() == 0 { return Ok(Line::BlankLine) }

        if let Ok(li) = line.parse::<LabeledInstruction>() {
            return Ok(Line::LabeledInstruction(li));
        }

        if let Ok(sd) = line.parse::<SymbolDeclaration>() {
            return Ok(Line::SymbolDeclaration(sd));
        }

        Err("Syntax error")?
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct LabeledInstruction {
    label: Option<String>,
    insn: Instruction,
}

impl FromStr for LabeledInstruction {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<LabeledInstruction, Self::Err> {
        let (label, s) = if let Some((label, rest)) = s.split_once(':') {
            let label = label.trim();
            if label.len() == 0 { Err("Label with length zero")? }
            if let Some(i) = label.find(|c: char| !c.is_ascii_alphanumeric()) {
                Err(format!("Bad character at offset {} in label '{}'", i, label))?
            }
            (Some(label.to_string()), rest.trim_start())
        } else { (None, s) };

        Ok(LabeledInstruction{ label, insn: s.parse::<Instruction>()? })
    }
}



#[derive(Clone, Debug, PartialEq, Eq)]
struct SymbolDeclaration {
    name: String,
    value: u16,
}

impl FromStr for SymbolDeclaration {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (name, value) = s.split_once('=').ok_or_else(|| format!("Missing equals sign"))?;
        Ok(SymbolDeclaration { name: name.trim().to_owned(),
                               value: u16::from_str_radix(value.trim(), 8).with_context(format!("Bad value for {}", name))? })
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse() {
        for (test, expect) in [(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;",           Line::BlankLine),
                               ("addr0:     NOP                  ; This is a comment.",
                                Line::LabeledInstruction(LabeledInstruction{ label:Some("addr0".to_string()),
                                                                             insn: Instruction::Jump { when: true,
                                                                                                       condition: Condition::NoOperation,
                                                                                                       effective_address: JumpDest::Absolute(0) } })),
                               ("           MV       #0,HEADLOAD ; This is also a comment",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::Move { source: MoveSource::Literal(0),
                                                                                                        dest: DestRegister::HeadLoadRegister } })),
                               ("           MV       GP9,GP5",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::Move { source: MoveSource::Register(SourceRegister::GPReg(9)),
                                                                                                        dest: DestRegister::GPReg(5) } })),
                               ("           JFC      addr13,TERMINATE; This is a comment with no space",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::Jump { when: false,
                                                                                                        condition: Condition::TerminateBar,
                                                                                                        effective_address: JumpDest::Symbol("addr13".to_owned()) } })),
                               ("           JTC      addr425,D5LOAD",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::Jump { when: true,
                                                                                                        condition: Condition::D5Load,
                                                                                                        effective_address: JumpDest::Symbol("addr425".to_owned()) } })),
                               ("           JUMP     addr33",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::Jump { when: false,
                                                                                                        condition: Condition::NoOperation,
                                                                                                        effective_address: JumpDest::Symbol("addr33".to_owned()) } })),
                               ("           WAIT     TIMER",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::Jump { when: false,
                                                                                                        condition: Condition::TimerDone,
                                                                                                        effective_address: JumpDest::Myself } })),
                               ("           CF       STEP,11MS",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::FunctionTimer { timer: Timer { negative_count: 247,
                                                                                                                                clock_rate: ClockRate::Microsecond },
                                                                                                                 function: Function::STEP } })),
                               ("           CF       ,11MS",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::FunctionTimer { timer: Timer { negative_count: 247,
                                                                                                                                clock_rate: ClockRate::Microsecond },
                                                                                                                 function: Function::NOP } })),
                               ("           F        RESETWRITE",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::FunctionALU { alu: None,
                                                                                                               function: Function::RESETWRITE } })),
                               ("           F        ,DEC",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::FunctionALU { alu: Some(ALUMode::DEC),
                                                                                                               function: Function::NOP } })),

                               ("           F        SETCONRDY,AND",
                                Line::LabeledInstruction(LabeledInstruction { label: None,
                                                                              insn: Instruction::FunctionALU { alu: Some(ALUMode::AND),
                                                                                                               function: Function::SETCONRDY } })),

                               ("           David = 777",
                                Line::SymbolDeclaration(SymbolDeclaration { name: "David".to_string(), value: 0o777 })),
        ].into_iter() {
            assert_eq!(test.parse::<Line>().expect("Parsing failed"), expect);
        }
    }

    #[test]
    fn emit_test() {
        let mut syms = SymbolTable::new();
        syms.insert("david".to_string(), 0xdc);

        for (expect, input) in vec![
            (0b01_10001_0_0000_0000, Instruction::Jump { when: false, condition: Condition::ACK, effective_address: JumpDest::Myself }),
            (0b00_11010_1_0010_1000, Instruction::Jump { when: true, condition: Condition::WriteProtect, effective_address: JumpDest::Absolute(0x128) }),
            (0b01_10011_0_1010_1010, Instruction::Jump { when: false, condition: Condition::ReadDataCmd, effective_address: JumpDest::Absolute(0xaa) }),
            (0b00_00110_0_1101_1100, Instruction::Jump { when: true, condition: Condition::IndexHole, effective_address: JumpDest::Symbol("david".to_string()) }),

            (0b10_10111_00_0_00_0000, Instruction::FunctionALU { alu: None, function: Function::NOP }),
            (0b10_00101_01_0_11_1001, Instruction::FunctionALU { alu: Some(ALUMode::XORBAR), function: Function::ENACRC }),
            (0b10_00011_1_0011_1100, Instruction::FunctionTimer { timer: Timer{ negative_count:60, clock_rate: ClockRate::Millisecond}, function: Function::WRITEGATE }),

            (0b11_1_110_11100_11010, Instruction::Move { source: MoveSource::Literal(0xdc), dest: DestRegister::GPReg(0xa) }),
            (0b11_0_000_11011_00101, Instruction::Move { source: MoveSource::Register(SourceRegister::GPReg(0xb)), dest: DestRegister::DriveSelReg }),
            (0b11_0_000_00101_01000, Instruction::Move { source: MoveSource::Register(SourceRegister::DReg), dest: DestRegister::HeadLoadRegister }),
        ].into_iter() {
            let words = emit(vec![input.clone()], syms.clone()).expect("emit failed for");
            if words[0] != expect {
                panic!("Emit mismatch for {0:?}\n  left: {1:5} {1:#08o} {1:#018b}\n right: {2:5} {2:#08o} {2:#018b}\n", input, expect, words[0]);
            }
        }
    }
}
