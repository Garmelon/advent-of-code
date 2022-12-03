#[derive(Clone, Copy, PartialEq, Eq)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

impl Choice {
    fn score(self) -> u32 {
        match self {
            Self::Rock => 1,
            Self::Paper => 2,
            Self::Scissors => 3,
        }
    }

    fn against(self, opponent: Self) -> Outcome {
        use self::Choice::*;
        match (self, opponent) {
            (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => Outcome::Lose,
            (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => Outcome::Draw,
            (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Outcome::Win,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Outcome {
    Lose,
    Draw,
    Win,
}

impl Outcome {
    fn score(self) -> u32 {
        match self {
            Outcome::Lose => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        }
    }

    fn against(self, opponent: Choice) -> Choice {
        match (self, opponent) {
            (Self::Lose, Choice::Rock) => Choice::Scissors,
            (Self::Lose, Choice::Paper) => Choice::Rock,
            (Self::Lose, Choice::Scissors) => Choice::Paper,
            (Self::Draw, c) => c,
            (Self::Win, Choice::Rock) => Choice::Paper,
            (Self::Win, Choice::Paper) => Choice::Scissors,
            (Self::Win, Choice::Scissors) => Choice::Rock,
        }
    }
}

fn read_round(line: &str) -> (Choice, Choice, Outcome) {
    let elems = line.split(' ').take(2).collect::<Vec<_>>();
    let l = match elems[0] {
        "A" => Choice::Rock,
        "B" => Choice::Paper,
        "C" => Choice::Scissors,
        _ => panic!(),
    };
    let (rc, ro) = match elems[1] {
        "X" => (Choice::Rock, Outcome::Lose),
        "Y" => (Choice::Paper, Outcome::Draw),
        "Z" => (Choice::Scissors, Outcome::Win),
        _ => panic!(),
    };
    (l, rc, ro)
}

pub fn solve(input: String) {
    let matches = input.lines().map(read_round).collect::<Vec<_>>();

    // Part 1
    let score = matches
        .iter()
        .map(|(l, r, _)| r.score() + r.against(*l).score())
        .sum::<u32>();
    println!("Part 1: {score}");

    // Part 2
    let score = matches
        .iter()
        .map(|(l, _, r)| r.against(*l).score() + r.score())
        .sum::<u32>();
    println!("Part 2: {score}");
}
