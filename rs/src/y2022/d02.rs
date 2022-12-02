#[derive(Clone, Copy, PartialEq, Eq)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

impl Choice {
    fn score_against(self, other: Self) -> u32 {
        use self::Choice::*;

        (match self {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        } + match (self, other) {
            (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 0,
            (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 3,
            (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => 6,
        })
    }
}

fn read_round(line: &str) -> (Choice, Choice) {
    let elems = line.split(' ').take(2).collect::<Vec<_>>();
    let l = match elems[0] {
        "A" => Choice::Rock,
        "B" => Choice::Paper,
        "C" => Choice::Scissors,
        _ => panic!(),
    };
    let r = match elems[1] {
        "X" => Choice::Rock,
        "Y" => Choice::Paper,
        "Z" => Choice::Scissors,
        _ => panic!(),
    };
    (l, r)
}

pub fn solve(input: String) -> anyhow::Result<()> {
    let choices = input
        .lines()
        .map(|l| read_round(l.trim()))
        .collect::<Vec<_>>();

    // Part 1
    let score = choices
        .iter()
        .map(|(l, r)| r.score_against(*l))
        .sum::<u32>();
    println!("Part 1: {score}");

    Ok(())
}
