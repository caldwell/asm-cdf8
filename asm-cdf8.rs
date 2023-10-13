// Copyright © 2023 David Caldwell <david@porkrind.org>

use docopt::Docopt;
use serde::Deserialize;

use std::error::Error;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::result::Result;

mod cdf8;
mod dis;
mod asm;

const USAGE: &'static str = r#"
Usage:
  asm-cdf8 -h
  asm-cdf8 [-v...] [-h] [-f <format>] [-i] [-o <image>] <source-file>
  asm-cdf8 [-v...] [-h] [--twoboard] -d <image>

Options:
  -h --help              Show this screen.
  -v --verbose           Be more verbose.
  -d --disassemble       Disassemble microcode image
  -o --output=<image>    Ouput image to <image> instead of stdout.
  -f --format=<format>   Set the output format. <format> can be one of:
                         hexdump, binary, ihex, srec  [default: hexdump]
  -i --interleave        Split the output into 2 byte-wide sections: msb and
                         lsb. If combined with --output, the output file
                         names will have ".msb" and ".lsb" appended.
  --twoboard             Use the older \"two board\" revision instruction set
"#;

#[derive(Debug, Deserialize)]
struct Args {
    flag_verbose:     usize,
    flag_disassemble: bool,
    flag_twoboard:    bool,
    flag_output:      Option<PathBuf>,
    flag_format:      Format,
    flag_interleave:  bool,
    arg_source_file:  PathBuf,
    arg_image:        PathBuf,
}

#[derive(Debug, Deserialize)]
#[allow(non_camel_case_types)]
enum Format {
    hexdump,
    binary,
    ihex,
    srec
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    if args.flag_verbose > 3 { println!("args={args:#?}") }

    if args.flag_disassemble {
        if args.flag_twoboard {
            dis::disassemble(&dis::TwoBoard::new(), &File::open(args.arg_image)?, &mut std::io::stdout(), args.flag_verbose > 0)?;
        } else {
            dis::disassemble(&dis::OneBoard::new(), &File::open(args.arg_image)?, &mut std::io::stdout(), args.flag_verbose > 0)?;
        };
    } else {
        let words = asm::assemble(&File::open(&args.arg_source_file).with_context(format!("source file '{}'", args.arg_source_file.to_string_lossy()))?)?;

        let bytes: Vec<u8> = words.into_iter().flat_map(|w| vec![(w>>8) as u8, (w & 0xff) as u8]).collect();

        fn maybe_clone_add_ext(maybe_path: &Option<PathBuf>, ext: &str) -> Option<PathBuf> {
            let Some(ref base) = maybe_path else { return None };
            let mut new = base.as_os_str().to_owned();
            new.push(ext);
            Some(PathBuf::from(new))
        }

        let outs: Vec<(&'static str, Vec<u8>, Option<PathBuf>)> =
            if args.flag_interleave {
                vec![("MSB", bytes.iter()        .step_by(2).map(|b| *b).collect(),            maybe_clone_add_ext(&args.flag_output, ".msb")),
                     ("LSB", bytes.iter().skip(1).step_by(2).map(|b| *b).collect::<Vec<u8>>(), maybe_clone_add_ext(&args.flag_output, ".lsb"))]
            } else {
                vec![("Big Endian Image", bytes, args.flag_output)]
            };

        let outs = match args.flag_format {
            Format::hexdump => outs.into_iter().map(|(k, o, n)| (k, pretty_hex::pretty_hex(&o).into_bytes(), n)).collect(),
            Format::binary => outs,
            Format::ihex => {
                outs.into_iter().map(|(k, o, n)| -> Result<_, Box<dyn Error>> {
                    let mut records: Vec<ihex::Record> = o.chunks(32)
                        .enumerate()
                        .map(|(i, chunk)| ihex::Record::Data { offset: i as u16 *32,
                                                               value: chunk.to_vec() }).collect();
                    records.push(ihex::Record::EndOfFile);

                    Ok((k, ihex::create_object_file_representation(&records)?.into_bytes(), n))
                }).collect::<Result<Vec<_>, Box<dyn Error>>>()?
            },
            Format::srec => {
                outs.into_iter().map(|(k, o, n)| -> Result<_, Box<dyn Error>> {
                    let mut records: Vec<srec::Record> = vec![srec::Record::S0("CDF8 ROM Image".into())];
                        records.extend(o.chunks(32)
                                       .enumerate()
                                       .map(|(i, chunk)| srec::Record::S1(srec::Data { address: srec::Address16(i as u16 * 32),
                                                                                       data: chunk.to_vec() })));
                    Ok((k, srec::writer::generate_srec_file(&records).into_bytes(), n))
                }).collect::<Result<Vec<_>, Box<dyn Error>>>()?
            },
        };

        for (kind, bytes, name) in outs {
            if let Some(name) = name {
                std::fs::write(&name, bytes).with_context(format!("output file '{}'", name.to_string_lossy()))?;
            } else {
                println!("{}:", kind);
                std::io::stdout().write(&bytes)?;
                println!("");
            }
        }
    }

    Ok(())
}

trait WithContext<T> {
    fn with_context(self, context: String)->Result<T, String>;
}

impl<T, E> WithContext<T> for Result<T, E> where E: std::fmt::Display {
    fn with_context(self, context: String)->Result<T, String> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(format!("{}: {}", context, e))
        }
    }
}
