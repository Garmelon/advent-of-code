mod d01;

use std::fs;
use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Day {
    D01,
}

#[derive(Parser)]
struct Args {
    day: Day,
    file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let input = fs::read_to_string(&args.file)?;

    match args.day {
        Day::D01 => d01::solve(input)?,
    }

    Ok(())
}
