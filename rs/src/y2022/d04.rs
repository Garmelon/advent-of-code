fn int(str: &str) -> u32 {
    str.parse().unwrap()
}

pub fn solve(input: String) {
    let pairs = input
        .lines()
        .map(|l| {
            let (r1, r2) = l.split_once(',').unwrap();
            let (s1, e1) = r1.split_once('-').unwrap();
            let (s2, e2) = r2.split_once('-').unwrap();
            (int(s1), int(e1), int(s2), int(e2))
        })
        .collect::<Vec<_>>();

    // Part 1
    let score = pairs
        .iter()
        .filter(|(s1, e1, s2, e2)| (s1 <= s2 && e2 <= e1) || (s2 <= s1 && e1 <= e2))
        .count();
    println!("Part 1: {score}");

    // Part 2
    let score = pairs
        .iter()
        .filter(|(s1, e1, s2, e2)| s1 <= e2 && s2 <= e1)
        .count();
    println!("Part 2: {score}");
}
