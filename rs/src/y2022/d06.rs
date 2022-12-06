pub fn solve(input: String) {
    let part1 = input
        .chars()
        .zip(input.chars().skip(1))
        .zip(input.chars().skip(2))
        .zip(input.char_indices().skip(3))
        .find_map(|(((c1, c2), c3), (i, c4))| {
            if c1 != c2 && c1 != c3 && c1 != c4 && c2 != c3 && c2 != c4 && c3 != c4 {
                println!("{c1}{c2}{c3}{c4}");
                Some(i + 1)
            } else {
                None
            }
        })
        .unwrap();
    println!("Part 1: {part1}");
}
