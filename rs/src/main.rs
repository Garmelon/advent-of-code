mod d01;

use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{fmt, fs};

use clap::Parser;

enum Day {
    Y2022D01,
}

impl Day {
    fn from_path(path: &Path) -> Option<Self> {
        Self::from_str(path.file_stem()?.to_str()?).ok()
    }
}

impl fmt::Display for Day {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Day::Y2022D01 => "2022_01",
        }
        .fmt(f)
    }
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "2022_01" => Self::Y2022D01,
            _ => return Err(()),
        })
    }
}

#[derive(Parser)]
struct Args {
    files: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if args.files.is_empty() {
        eprintln!("No days specified");
    }

    let mut first_day = true;
    for file in args.files {
        if !first_day {
            println!();
        }
        first_day = false;

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
            Day::Y2022D01 => d01::solve(input)?,
        }
    }

    Ok(())
}
