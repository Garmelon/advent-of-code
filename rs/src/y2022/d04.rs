use std::ops::RangeInclusive;

fn contains(a: &RangeInclusive<u32>, b: &RangeInclusive<u32>) -> bool {
    a.start() <= b.start() && b.end() <= a.end()
}

pub fn solve(input: String) {
    let pairs = input
        .lines()
        .map(|l| {
            let mut ranges = l.split(',').map(|p| {
                let mut numbers = p.split('-').map(|n| n.parse::<u32>().unwrap());
                numbers.next().unwrap()..=numbers.next().unwrap()
            });
            (ranges.next().unwrap(), ranges.next().unwrap())
        })
        .collect::<Vec<_>>();

    let score = pairs
        .iter()
        .filter(|(a, b)| contains(a, b) || contains(b, a))
        .count();
    println!("Part 1: {score}");
}
