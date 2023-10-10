// Copyright © 2023 David Caldwell <david@porkrind.org>

use docopt::Docopt;
use serde::Deserialize;

use std::error::Error;
use std::fs::File;
use std::path::PathBuf;
use std::result::Result;

mod cdf8;
mod dis;
mod asm;

const USAGE: &'static str = "
Usage:
  asm-cdf8 -h
  asm-cdf8 [-v...] [-h] [-o <image>] <source-file>
  asm-cdf8 [-v...] [-h] [--twoboard] -d <image>

Options:
  -h --help              Show this screen.
  -v --verbose           Be more verbose.
  -d --disassemble       Disassemble microcode image
  -o --output=<image>    Ouput image to <image> (default: a.out)
  --twoboard             Use the older \"two board\" revision instruction set
";

#[derive(Debug, Deserialize)]
struct Args {
    flag_verbose:     usize,
    flag_disassemble: bool,
    flag_twoboard:    bool,
    arg_source_file:  PathBuf,
    arg_image:        PathBuf,
}


fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    if args.flag_verbose > 3 { println!("args={args:#?}") }

    if args.flag_disassemble {
        if args.flag_twoboard {
            dis::disassemble(&dis::TwoBoard::new(), &File::open(args.arg_image)?, &mut std::io::stdout())?;
        } else {
            dis::disassemble(&dis::OneBoard::new(), &File::open(args.arg_image)?, &mut std::io::stdout())?;
        };
    } else {
        asm::assemble(&File::open(args.arg_source_file)?, &mut File::create(args.arg_image)?)?;
    }

    Ok(())
}
