mod y2022;

use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{fmt, fs, io};

use clap::Parser;

macro_rules! days {
    ( $( $day:ident : $name:expr, )* ) => {
        enum Day { $( $day, )* }

        impl fmt::Display for Day{
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
    };
}

impl Day {
    fn from_path(path: &Path) -> Option<Self> {
        Self::from_str(path.file_stem()?.to_str()?).ok()
    }
}

days! {
    Y2022D01: "2022_01",
    Y2022D02: "2022_02",
    Y2022D03: "2022_03",
    Y2022D04: "2022_04",
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
                eprintln!("### Could not determine day: {file:?}");
                continue;
            }
        };

        println!("### Solving day {day}");
        let input = fs::read_to_string(file)?;
        match day {
            Day::Y2022D01 => y2022::d01::solve(input),
            Day::Y2022D02 => y2022::d02::solve(input),
            Day::Y2022D03 => y2022::d03::solve(input),
            Day::Y2022D04 => y2022::d04::solve(input),
        }
        println!()
    }

    Ok(())
}
