use std::collections::HashSet;

fn score(c: char) -> u32 {
    match c {
        'a'..='z' => c as u32 - 'a' as u32 + 1,
        'A'..='Z' => c as u32 - 'A' as u32 + 27,
        _ => panic!(),
    }
}

pub fn solve(input: String) -> anyhow::Result<()> {
    let backpacks = input
        .lines()
        .map(|l| l.trim())
        .map(|l| l.split_at(l.len() / 2))
        .collect::<Vec<_>>();

    // Part 1
    let score = backpacks
        .iter()
        .map(|(l, r)| {
            let l = l.chars().collect::<HashSet<_>>();
            let r = r.chars().collect::<HashSet<_>>();
            l.intersection(&r).map(|c| score(*c)).sum::<u32>()
        })
        .sum::<u32>();
    println!("Part 1: {score}");

    Ok(())
}
