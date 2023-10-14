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
  asm-cdf8 [-h] [-f <format>] [(-o <image> | --msb <msb-image> --lsb <lsb-image>)] <source-file>
  asm-cdf8 [-h] -d [--twoboard] [--nodump] [--details] (<image> | --msb <msb-image> --lsb <lsb-image>)

Options:
  -h --help              Show this screen.

 Asssembly Options:
  -o --output=<image>    Ouput image to <image> instead of stdout.
  -f --format=<format>   Set the output format. <format> can be one of:
                         hexdump, binary, ihex, srec  [default: hexdump]

 Common Options:
  -m --msb=<msb-image>   When assembling: Split the output into 2 sections
  -l --lsb=<lsb-image>   that are 1 byte wide: msb and lsb.
                         When disassembling: Interleave the msb and lsb input
                         images before disassembling.
 Disassembly Options:
  -d --disassemble       Disassemble microcode image
  -2 --twoboard          Use the older "two board" revision instruction set
  -n --nodump            Don't output the raw address and instruction values
  -c --details           Include a list of processor "constants" up front
"#;

#[derive(Debug, Deserialize)]
struct Args {
    flag_disassemble: bool,
    flag_twoboard:    bool,
    flag_nodump:      bool,
    flag_details:     bool,
    flag_output:      Option<PathBuf>,
    flag_format:      Format,
    flag_msb:         Option<PathBuf>,
    flag_lsb:         Option<PathBuf>,
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

    if args.flag_disassemble {
        let image = if let (Some(msb), Some(lsb)) = (args.flag_msb, args.flag_lsb) {
            if std::fs::metadata(&msb)?.len() != std::fs::metadata(&lsb)?.len() {
                Err(format!("Can't interleave {} and {}: files have unequal lengths",
                            msb.to_string_lossy(), lsb.to_string_lossy()))?
            }
            std::fs::read(msb)?.into_iter().zip(std::fs::read(lsb)?.into_iter()).flat_map(|(msb,lsb)| [msb,lsb]).collect()
        } else {
            std::fs::read(args.arg_image)?
        };
        if args.flag_twoboard {
            dis::disassemble(&dis::TwoBoard::new(), &image, &mut std::io::stdout(), !args.flag_nodump, args.flag_details)?;
        } else {
            dis::disassemble(&dis::OneBoard::new(), &image, &mut std::io::stdout(), !args.flag_nodump, args.flag_details)?;
        };
    } else {
        let words = asm::assemble(&File::open(&args.arg_source_file).with_context(format!("source file '{}'", args.arg_source_file.to_string_lossy()))?)?;

        let bytes: Vec<u8> = words.into_iter().flat_map(|w| vec![(w>>8) as u8, (w & 0xff) as u8]).collect();

        let outs: Vec<(&'static str, Vec<u8>, Option<PathBuf>)> =
            if let (Some(msb), Some(lsb)) = (args.flag_msb, args.flag_lsb) {
                vec![("MSB", bytes.iter()        .step_by(2).map(|b| *b).collect(),            Some(msb)),
                     ("LSB", bytes.iter().skip(1).step_by(2).map(|b| *b).collect::<Vec<u8>>(), Some(lsb))]
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn end_to_end_test() {
        let mut dis_bytes: Vec<u8> = vec![];
        let image: [u8; 970] = [
            0x00, 0x00, 0xb0, 0x00, 0x90, 0x00, 0x8c, 0x00, 0xe0, 0x08, 0x64, 0x0b, 0x73, 0x15, 0x21, 0x15, 0xba, 0x00, 0x9e, 0x00, 0xaa, 0x00, 0x12, 0x0e, 0xc2, 0x58, 0xe0, 0x15, 0x2e, 0x11, 0xc2, 0x59,
            0xe0, 0x35, 0x64, 0x15, 0xc0, 0x22, 0xb4, 0x00, 0xa6, 0x00, 0xac, 0x00, 0xa4, 0x00, 0xa0, 0x00, 0x40, 0x1b, 0xb1, 0xfa, 0x5a, 0x29, 0xe0, 0x37, 0xc3, 0xa3, 0xae, 0xaf, 0xc0, 0xbd, 0x3c, 0x25,
            0xe0, 0x17, 0xc3, 0x83, 0xae, 0xaf, 0xc0, 0xbc, 0x7c, 0x19, 0xc2, 0xe5, 0xff, 0xf5, 0xe0, 0x08, 0x40, 0x1b, 0x72, 0x1a, 0x06, 0xf3, 0x08, 0xf3, 0xc0, 0xc5, 0xc0, 0xc3, 0xe0, 0x64, 0x9a, 0xbb,
            0xc0, 0xa3, 0xc2, 0xa4, 0xae, 0xa6, 0x3c, 0x35, 0xaf, 0xf8, 0x12, 0x38, 0xc3, 0x12, 0xf9, 0x1c, 0x2e, 0x3b, 0xc3, 0x32, 0xf9, 0x1d, 0x1e, 0x3d, 0xe0, 0x28, 0x2d, 0x75, 0x2a, 0x43, 0xa6, 0x00,
            0x62, 0x40, 0xc0, 0x43, 0xe3, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa6, 0xbb, 0xc0, 0xb3, 0x62, 0x48, 0xc0, 0x43, 0xef, 0xe4, 0xae, 0xbb, 0xc0, 0xb1, 0x0a, 0x73, 0xff, 0xb7, 0xff, 0x75,
            0xff, 0x7a, 0xc2, 0x23, 0xe9, 0x84, 0xae, 0xa6, 0x7a, 0x94, 0xc2, 0x44, 0x82, 0x86, 0x3a, 0x66, 0xc0, 0xa3, 0x80, 0xaf, 0x3c, 0x68, 0x85, 0xf7, 0x5a, 0x5c, 0xaf, 0xf7, 0x78, 0x58, 0x40, 0x67,
            0x85, 0xf7, 0x5a, 0x61, 0xaf, 0xf7, 0xc0, 0xa3, 0x82, 0x80, 0x7a, 0x68, 0x78, 0x60, 0x41, 0x68, 0x5a, 0x68, 0xc2, 0x32, 0x2a, 0x0b, 0x4e, 0x6b, 0xff, 0xc4, 0x8c, 0x00, 0x4c, 0x73, 0xc3, 0x43,
            0xae, 0x80, 0x3c, 0x94, 0xc0, 0xba, 0x8a, 0x00, 0x42, 0x74, 0x54, 0x6c, 0xc0, 0x03, 0xae, 0xa6, 0x7c, 0x6c, 0x42, 0x79, 0xc0, 0x16, 0x42, 0x7b, 0x42, 0x7c, 0xc0, 0x14, 0x42, 0x7e, 0x42, 0x7f,
            0x42, 0x80, 0x42, 0x81, 0x50, 0x85, 0xb8, 0x00, 0x40, 0x6c, 0xc2, 0xd2, 0xc2, 0x43, 0xc2, 0x24, 0x8c, 0xa6, 0x7c, 0x8f, 0xc2, 0x83, 0xc2, 0x64, 0xae, 0xa6, 0x3c, 0x96, 0x40, 0x6c, 0xba, 0x00,
            0xc2, 0xa3, 0xae, 0x80, 0xc0, 0xb5, 0x3a, 0x51, 0x94, 0x00, 0x40, 0x0a, 0xe0, 0x06, 0x28, 0xce, 0x36, 0xce, 0x8b, 0xfd, 0x42, 0x9a, 0x54, 0xa3, 0xff, 0x64, 0xc0, 0x03, 0xae, 0xa6, 0x3c, 0xbc,
            0xff, 0x04, 0xae, 0xa6, 0x3c, 0xbb, 0x8c, 0x00, 0x8a, 0x00, 0x5a, 0x9a, 0xb8, 0x00, 0xc2, 0xe3, 0x8c, 0x80, 0x81, 0xec, 0x3c, 0xaf, 0x82, 0xaf, 0x7c, 0xb6, 0x98, 0x00, 0x40, 0x0a, 0xc0, 0xb7,
            0xc2, 0x43, 0xe9, 0x84, 0xae, 0x86, 0x7a, 0x4f, 0x84, 0x00, 0x40, 0xb9, 0x38, 0xb8, 0x84, 0x80, 0xc0, 0xb7, 0x5a, 0xb9, 0x40, 0x4f, 0xa2, 0x00, 0xf0, 0x03, 0xc0, 0xe0, 0x42, 0xbe, 0x28, 0xc2,
            0x36, 0xc2, 0xc0, 0x07, 0xb2, 0x80, 0xc0, 0xa3, 0x3a, 0xbd, 0x28, 0xe7, 0x36, 0xe7, 0x42, 0xc7, 0x42, 0xc8, 0x42, 0xc9, 0x10, 0xa6, 0x8c, 0x00, 0x20, 0xf3, 0x40, 0x0b, 0x34, 0xf1, 0x89, 0xfd,
            0x86, 0x00, 0xff, 0xe1, 0xff, 0xe0, 0xfe, 0xa3, 0x1a, 0x0a, 0x42, 0xd4, 0xae, 0x80, 0xc0, 0xa3, 0x7c, 0xd4, 0xff, 0x43, 0xe0, 0x00, 0x42, 0xdb, 0xae, 0x80, 0xc0, 0xa3, 0x3a, 0xdb, 0x8a, 0x00,
            0xff, 0x00, 0x36, 0xe3, 0xff, 0x60, 0xf8, 0xe1, 0x42, 0xe4, 0xff, 0xe1, 0x40, 0xbc, 0x8e, 0x00, 0x42, 0xe8, 0x42, 0xe9, 0x90, 0x00, 0xff, 0xe0, 0x42, 0xec, 0x42, 0xed, 0xb0, 0x00, 0x8c, 0x00,
            0x40, 0x0b, 0x9c, 0x00, 0x40, 0x0a, 0xe0, 0x06, 0xf0, 0x03, 0xc0, 0xe2, 0xa6, 0x00, 0x62, 0xf7, 0x46, 0xfa, 0xc0, 0x47, 0xb2, 0x80, 0xc0, 0xa3, 0x3a, 0xf5, 0x60, 0x15, 0xc2, 0x83, 0xe0, 0x84,
            0xae, 0xa9, 0xc0, 0xb3, 0xc0, 0xa3, 0xe3, 0x64, 0xae, 0x86, 0x3b, 0x14, 0xe3, 0x84, 0xae, 0xa6, 0x3d, 0x10, 0xae, 0x86, 0x7d, 0x0d, 0xae, 0x80, 0xc0, 0xa3, 0xe3, 0x44, 0xae, 0x86, 0x41, 0x01,
            0xe0, 0x33, 0xc2, 0x43, 0xae, 0x80, 0xc0, 0xb1, 0x40, 0x4d, 0xaf, 0xfe, 0x1a, 0x0a, 0x5b, 0x17, 0xaf, 0xf8, 0xe0, 0x03, 0xe0, 0x06, 0xe0, 0x07, 0xb2, 0x80, 0xc0, 0xa3, 0x3b, 0x1b, 0xe0, 0x10,
            0xc0, 0xe4, 0xae, 0xa6, 0x7c, 0x0a, 0xff, 0xe7, 0xc0, 0xe4, 0xae, 0xba, 0x7c, 0x0a, 0xc2, 0x03, 0xb2, 0x80, 0xc0, 0xb0, 0xe0, 0x03, 0x3b, 0x20, 0x1a, 0x0a, 0xc2, 0x1e, 0xc3, 0xd3, 0xc2, 0x7c,
            0xc3, 0x94, 0xc2, 0x9b, 0xc3, 0x76, 0xc2, 0xda, 0xc3, 0x57, 0xc2, 0xf8, 0xc3, 0x15, 0xc2, 0xb9, 0xc3, 0x3d, 0xc3, 0xb2, 0xc2, 0x5f, 0xc3, 0xf1, 0xc2, 0x27, 0xc0, 0xe4, 0xae, 0xa6, 0x7c, 0x0a,
            0xb2, 0x80, 0xc0, 0xa3, 0xc0, 0xb0, 0x3b, 0x2d, 0x5a, 0x0a, 0xe0, 0x06, 0xc0, 0xe4, 0xae, 0xa6, 0x7c, 0x0a, 0xb2, 0x80, 0xc0, 0xa3, 0x3b, 0x46, 0xb6, 0x00, 0xe0, 0x50, 0xe0, 0x11, 0x41, 0x52,
            0x9e, 0x00, 0xc2, 0x11, 0xc2, 0x03, 0xe0, 0x08, 0xae, 0xaf, 0xc0, 0xb0, 0x3d, 0x6b, 0xc2, 0x05, 0xe0, 0x28, 0x0b, 0x50, 0xe1, 0x43, 0x80, 0xaf, 0x85, 0xf7, 0x5b, 0x5d, 0xc0, 0xa3, 0x7d, 0x5b,
            0x39, 0x68, 0xea, 0x63, 0x82, 0xaf, 0x39, 0x52, 0x85, 0xf7, 0x5b, 0x65, 0xc0, 0xa3, 0x7d, 0x62, 0xba, 0x00, 0x96, 0x00, 0x40, 0x0a, 0xc2, 0x25, 0xe0, 0x18, 0xe0, 0x19, 0xe0, 0x31, 0xe0, 0x33,
            0xe0, 0x12, 0x60, 0x0b, 0xe0, 0x05, 0xe0, 0x28, 0x41, 0x14, 0xa7, 0xfc, 0x1b, 0x68, 0x63, 0x76, 0xc0, 0x43, 0xe2, 0x24, 0xae, 0xa6, 0x7d, 0x6a, 0x34, 0xf1, 0x0a, 0x08, 0xe9, 0xe3, 0x39, 0x86,
            0x82, 0xaf, 0x85, 0xf0, 0x5b, 0x82, 0xc0, 0xa3, 0x7d, 0x7f, 0x41, 0x68, 0x4f, 0x86, 0xe0, 0x12, 0xe0, 0x34, 0x0d, 0x8a, 0x0a, 0x00, 0x4d, 0x8a, 0xe5, 0xc3, 0xe0, 0x00, 0xff, 0xe1, 0x88, 0x00,
            0x86, 0x00, 0x43, 0x91, 0xae, 0xaf, 0xc0, 0xa3, 0x7d, 0x91, 0xfa, 0xe1, 0xff, 0x80, 0x43, 0x97, 0xe0, 0x00, 0xff, 0xe1, 0xe4, 0x03, 0xae, 0xaf, 0xc0, 0xa3, 0x3d, 0xa0, 0x43, 0x9e, 0x41, 0x9b,
            0x8a, 0x00, 0xf8, 0xe1, 0xff, 0xc0, 0x43, 0xa3, 0xff, 0xe1, 0xc2, 0x40, 0x43, 0xa6, 0xe0, 0x00, 0x43, 0xa8, 0xc2, 0x80, 0x43, 0xaa, 0xe0, 0x00, 0x43, 0xac, 0x8e, 0x00, 0x43, 0xae, 0x43, 0xaf,
            0x90, 0x00, 0xff, 0xe0, 0xfe, 0xa3, 0x43, 0xb3, 0x8c, 0x80, 0xc0, 0xa3, 0x3b, 0xb3, 0xe0, 0x00, 0xff, 0x43, 0x43, 0xb9, 0xae, 0x80, 0xc0, 0xa3, 0x3b, 0xb9, 0x8a, 0x00, 0xf8, 0xe1, 0xff, 0x60,
            0x43, 0xc0, 0xff, 0xe1, 0xfc, 0xa0, 0xf0, 0x03, 0x43, 0xc4, 0xae, 0x80, 0xc0, 0xa3, 0x3b, 0xc4, 0x8e, 0x00, 0x43, 0xc9, 0x43, 0xca, 0x90, 0x00, 0xe0, 0x00, 0xe4, 0x23, 0x43, 0xce, 0x8c, 0x00,
            0xae, 0xaf, 0xc0, 0xa3, 0x7d, 0xce, 0xc2, 0x83, 0xae, 0x80, 0xc0, 0xb4, 0xfc, 0xa4, 0xae, 0xa9, 0x7d, 0xa0, 0x4d, 0xd9, 0xc2, 0x43, 0xe9, 0x84, 0xb0, 0x80, 0xc0, 0xb2, 0x80, 0xa6, 0x3d, 0xe3,
            0x85, 0xec, 0x5b, 0xe1, 0x41, 0x88, 0xe9, 0x92, 0x40, 0x0b
        ];

        dis::disassemble(&dis::OneBoard::new(), &image[..], &mut dis_bytes, false, false).expect("disassemble failed");
        println!("disassembly={}", String::from_utf8(dis_bytes.clone()).expect("utf8"));
        let image_out = asm::assemble(&dis_bytes[..]).expect("assemble failed");

        assert_eq!(2*image_out.len(), image.len());

        for (i, byte) in image_out.into_iter().flat_map(|w| [(w >> 8) as u8,w as u8]).enumerate() {
            if image[i] != byte { panic!("byte #{0} doesn't match\n  left={1:#04x} {1:08b}\n right={2:#04x} {2:08b}\n", i, image[i], byte); }
        }
    }
}
