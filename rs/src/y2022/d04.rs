pub fn solve(input: String) {
    let pairs = input
        .lines()
        .map(|l| {
            let mut ranges = l.split(',').map(|p| {
                let mut numbers = p.split('-').map(|n| n.parse::<u32>().unwrap());
                (numbers.next().unwrap(), numbers.next().unwrap())
            });
            (ranges.next().unwrap(), ranges.next().unwrap())
        })
        .collect::<Vec<_>>();

    // Part 1
    let score = pairs
        .iter()
        .filter(|((s1, e1), (s2, e2))| (s1 <= s2 && e2 <= e1) || (s2 <= s1 && e1 <= e2))
        .count();
    println!("Part 1: {score}");

    // Part 2
    let score = pairs
        .iter()
        .filter(|((s1, e1), (s2, e2))| s1 <= e2 && s2 <= e1)
        .count();
    println!("Part 2: {score}");
}
