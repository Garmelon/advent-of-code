use std::collections::HashSet;

fn scan(chars: &[char], lookback: usize) -> usize {
    chars
        .windows(lookback)
        .position(|w| w.iter().copied().collect::<HashSet<_>>().len() == lookback)
        .unwrap()
        + lookback
}

pub fn solve(input: String) {
    let chars = input.chars().collect::<Vec<_>>();
    println!("Part 1: {}", scan(&chars, 4));
    println!("Part 2: {}", scan(&chars, 14));
}
