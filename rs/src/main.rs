mod y2022;

use std::os::unix::prelude::OsStrExt;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{fmt, fs, io};

use clap::Parser;

macro_rules! days {
    ( $( $day:ident : $name:expr => $path:path, )* ) => {
        enum Day { $( $day, )* }

        impl fmt::Display for Day {
            fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $( Self::$day => $name, )*
                }.fmt(f)
            }
        }

        impl FromStr for Day {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(match s {
                    $( $name => Self::$day, )*
                    _ => return Err(()),
                })
            }
        }

        impl Day {
            fn solve(self, input: String) {
                match self {
                    $( Self::$day => $path(input), )*
                }
            }
        }
    };
}

impl Day {
    fn from_path(path: &Path) -> Option<Self> {
        let bytes = path.file_stem()?.as_bytes();
        if bytes.len() < 7 {
            return None;
        }
        let day = String::from_utf8_lossy(&bytes[..7]);
        Self::from_str(&day).ok()
    }
}

days! {
    Y2022D01: "2022_01" => y2022::d01::solve,
    Y2022D02: "2022_02" => y2022::d02::solve,
    Y2022D03: "2022_03" => y2022::d03::solve,
    Y2022D04: "2022_04" => y2022::d04::solve,
    Y2022D05: "2022_05" => y2022::d05::solve,
    Y2022D06: "2022_06" => y2022::d06::solve,
    Y2022D07: "2022_07" => y2022::d07::solve,
    Y2022D08: "2022_08" => y2022::d08::solve,
    Y2022D09: "2022_09" => y2022::d09::solve,
    Y2022D10: "2022_10" => y2022::d10::solve,
}

#[derive(Parser)]
struct Args {
    #[arg(required = true)]
    files: Vec<PathBuf>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    for file in args.files {
        let day = match Day::from_path(&file) {
            Some(day) => day,
            None => {
                eprintln!("### Can't solve {file:?}");
                continue;
            }
        };

        eprintln!("### Solving {file:?}");
        let input = fs::read_to_string(file)?;
        day.solve(input);
        eprintln!()
    }

    Ok(())
}
