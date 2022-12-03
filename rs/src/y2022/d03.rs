fn parse_item(c: char) -> u64 {
    let n = match c {
        'a'..='z' => c as u64 - 'a' as u64,
        'A'..='Z' => c as u64 - 'A' as u64 + 26,
        _ => panic!(),
    };
    1 << n
}

fn parse_items(s: &str) -> u64 {
    s.chars().map(parse_item).reduce(|a, b| a | b).unwrap_or(0)
}

// Returns the score of the item with the highest score
fn calc_score(i: u64) -> u32 {
    64 - i.leading_zeros()
}

pub fn solve(input: String) -> anyhow::Result<()> {
    let backpacks = input.lines().map(|l| l.trim()).collect::<Vec<_>>();

    // Part 1
    let score = backpacks
        .iter()
        .map(|backpack| {
            let (l, r) = backpack.split_at(backpack.len() / 2);
            calc_score(parse_items(l) & parse_items(r))
        })
        .sum::<u32>();
    println!("Part 1: {score}");

    // Part 2
    let score = backpacks
        .chunks(3)
        .map(|chunk| {
            calc_score(
                chunk
                    .iter()
                    .map(|i| parse_items(i))
                    .reduce(|a, b| a & b)
                    .unwrap(),
            )
        })
        .sum::<u32>();
    println!("Part 2: {score}");

    Ok(())
}
