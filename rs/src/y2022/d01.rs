pub fn solve(input: String) -> anyhow::Result<()> {
    let mut elves = input
        .trim()
        .split("\n\n")
        .map(|chunk| {
            chunk
                .split('\n')
                .map(|n| n.parse::<u32>().unwrap())
                .sum::<u32>()
        })
        .collect::<Vec<_>>();
    elves.sort_unstable();

    // Part 1
    let top = elves.last().unwrap();
    println!("Part 1: {top}");

    // Part 2
    let top_three = elves.iter().rev().take(3).sum::<u32>();
    println!("Part 2: {top_three}");

    Ok(())
}
