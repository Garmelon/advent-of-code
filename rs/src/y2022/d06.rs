use core::panic;
use std::collections::{HashSet, VecDeque};

fn scan(input: &str, lookback: usize) -> usize {
    let mut last_n = VecDeque::with_capacity(lookback);
    for (i, c) in input.chars().enumerate() {
        last_n.push_back(c);
        while last_n.len() > lookback {
            last_n.pop_front();
        }
        if last_n.len() < lookback {
            continue;
        }
        let last_n_set = last_n.iter().copied().collect::<HashSet<_>>();
        if last_n_set.len() == lookback {
            return i;
        }
    }
    panic!()
}

pub fn solve(input: String) {
    println!("Part 1: {}", scan(&input, 4) + 1);
    println!("Part 2: {}", scan(&input, 14) + 1);
}
