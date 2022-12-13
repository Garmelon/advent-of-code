use std::cmp::Ordering;
use std::iter::{self, Peekable};
use std::str::Chars;

#[derive(Clone)]
enum Message {
    Int(u32),
    List(Vec<Message>),
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

    let divp1 = vec![Message::List(vec![Message::Int(2)])];
    let divp2 = vec![Message::List(vec![Message::Int(6)])];
    let mut packets = pairs
        .into_iter()
        .flat_map(|(a, b)| iter::once(a).chain(iter::once(b)))
        .chain(iter::once(divp1.clone()))
        .chain(iter::once(divp2.clone()))
        .collect::<Vec<_>>();
    packets.sort();
    let divp1i = packets
        .iter()
        .enumerate()
        .find(|(_, p)| *p == &divp1)
        .unwrap()
        .0;
    let divp2i = packets
        .iter()
        .enumerate()
        .find(|(_, p)| *p == &divp2)
        .unwrap()
        .0;
    println!("Part 2: {}", (divp1i + 1) * (divp2i + 1));
}
