mod y2022;

use std::os::unix::prelude::OsStrExt;
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
        let day = &path.file_stem()?.as_bytes()[..7];
        let day = String::from_utf8_lossy(day);
        Self::from_str(&day).ok()
    }
}

days! {
    Y2022D01: "2022_01",
    Y2022D02: "2022_02",
    Y2022D03: "2022_03",
    Y2022D04: "2022_04",
    Y2022D05: "2022_05",
    Y2022D06: "2022_06",
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
        match day {
            Day::Y2022D01 => y2022::d01::solve(input),
            Day::Y2022D02 => y2022::d02::solve(input),
            Day::Y2022D03 => y2022::d03::solve(input),
            Day::Y2022D04 => y2022::d04::solve(input),
            Day::Y2022D05 => y2022::d05::solve(input),
            Day::Y2022D06 => y2022::d06::solve(input),
        }
        eprintln!()
    }

    Ok(())
}
