use std::cmp::Ordering;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

enum Message {
    Int(u32),
    List(Vec<Message>),
}

impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => int.fmt(f),
            Self::List(list) => f.debug_list().entries(list).finish(),
        }
    }
}

impl Ord for Message {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x.cmp(y),
            (Self::Int(x), Self::List(ys)) => vec![Self::Int(*x)].cmp(ys),
            (Self::List(xs), Self::Int(y)) => xs.cmp(&vec![Self::Int(*y)]),
            (Self::List(xs), Self::List(ys)) => xs.cmp(ys),
        }
    }
}

impl PartialOrd for Message {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Message {}

impl PartialEq for Message {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

type CharIter<'a> = Peekable<Chars<'a>>;

fn parse_int(chars: &mut CharIter) -> u32 {
    let mut result = 0;
    while let Some(char) = chars.peek() {
        if !char.is_ascii_digit() {
            break;
        }
        result = result * 10 + (*char as u32 - '0' as u32);
        chars.next();
    }
    result
}

fn parse_list(chars: &mut CharIter) -> Vec<Message> {
    let mut result = vec![];
    assert_eq!(chars.next(), Some('['));
    if !matches!(chars.peek(), Some(']')) {
        result.push(parse_message(chars));
        while matches!(chars.peek(), Some(',')) {
            chars.next();
            result.push(parse_message(chars));
        }
    }
    assert_eq!(chars.next(), Some(']'));
    result
}

fn parse_message(chars: &mut CharIter) -> Message {
    if matches!(chars.peek(), Some('[')) {
        Message::List(parse_list(chars))
    } else {
        Message::Int(parse_int(chars))
    }
}

pub fn solve(input: String) {
    let pairs = input
        .trim()
        .split("\n\n")
        .map(|p| {
            let (fst, snd) = p.split_once('\n').unwrap();
            let fst = parse_list(&mut fst.chars().peekable());
            let snd = parse_list(&mut snd.chars().peekable());
            (fst, snd)
        })
        .collect::<Vec<_>>();

    let part1 = pairs
        .iter()
        .enumerate()
        .filter(|(_, (fst, snd))| fst < snd)
        .map(|(i, _)| i + 1)
        .sum::<usize>();
    println!("Part 1: {part1}");
}
