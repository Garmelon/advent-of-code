pub fn solve(input: String) -> anyhow::Result<()> {
    let mut elves = vec![];
    let mut elf = vec![];
    for line in input.lines() {
        if let Ok(number) = line.trim().parse::<u32>() {
            elf.push(number);
        } else {
            elves.push(elf);
            elf = vec![];
        }
    }
    if !elf.is_empty() {
        elves.push(elf);
    }

    let mut elves = elves
        .into_iter()
        .map(|e| e.into_iter().sum())
        .collect::<Vec<u32>>();
    elves.sort_unstable();

    // Part 1
    let top = elves.last().unwrap();
    println!("Part 1: {top}");

    // Part 2
    let top_three = elves.iter().rev().take(3).sum::<u32>();
    println!("Part 2: {top_three}");

    Ok(())
}
