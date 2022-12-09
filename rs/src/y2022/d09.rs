use std::collections::HashSet;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
    Down,
    Up,
}

impl Direction {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "L" => Some(Self::Left),
            "R" => Some(Self::Right),
            "D" => Some(Self::Down),
            "U" => Some(Self::Up),
            _ => None,
        }
    }

    fn move_head(self, head: (i32, i32)) -> (i32, i32) {
        match self {
            Self::Left => (head.0 - 1, head.1),
            Self::Right => (head.0 + 1, head.1),
            Self::Down => (head.0, head.1 - 1),
            Self::Up => (head.0, head.1 + 1),
        }
    }

    fn move_tail(self, anchor: (i32, i32), tail: (i32, i32)) -> (i32, i32) {
        match self {
            Self::Left if tail.0 > anchor.0 + 1 => (anchor.0 + 1, anchor.1),
            Self::Right if tail.0 < anchor.0 - 1 => (anchor.0 - 1, anchor.1),
            Self::Down if tail.1 > anchor.1 + 1 => (anchor.0, anchor.1 + 1),
            Self::Up if tail.1 < anchor.1 - 1 => (anchor.0, anchor.1 - 1),
            _ => tail,
        }
    }
}

pub fn solve(input: String) {
    // +x points right, +y points up
    let mut head = (0, 0);
    let mut tail = (0, 0);
    let mut tail_trail = HashSet::new();

    for line in input.lines() {
        let (dir, amount) = line.split_once(' ').unwrap();
        let dir = Direction::from_str(dir).unwrap();
        let amount = amount.parse::<i32>().unwrap();
        for _ in 0..amount {
            head = dir.move_head(head);
            tail = dir.move_tail(head, tail);
            tail_trail.insert(tail);
        }
    }

    println!("Part 1: {}", tail_trail.len());
}
