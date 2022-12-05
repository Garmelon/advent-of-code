fn parse_stack_line(line: &str, stacks: &mut Vec<Vec<char>>) {
    let len = (line.len() + 1) / 4;
    while stacks.len() < len {
        stacks.push(vec![]);
    }
    let line = line.chars().collect::<Vec<_>>();
    for (i, chunk) in line[..].chunks(4).enumerate() {
        if chunk[0] == '[' {
            stacks[i].push(chunk[1]);
        }
    }
}

pub fn solve(input: String) {
    // Parse stacks
    let mut stacks = vec![];
    let mut lines = input.lines();
    for line in &mut lines {
        if line.is_empty() {
            break;
        }
        parse_stack_line(line, &mut stacks);
    }
    for stack in &mut stacks {
        stack.reverse();
    }

    // Parse moves
    let mut moves: Vec<(usize, usize, usize)> = vec![];
    for line in lines {
        let parts = line.split_ascii_whitespace().collect::<Vec<_>>();
        let amount = parts[1].parse().unwrap();
        let from = parts[3].parse().unwrap();
        let to = parts[5].parse().unwrap();
        moves.push((amount, from, to));
    }

    // Part 1
    let mut part1 = stacks.clone();
    for &(amount, from, to) in &moves {
        for _ in 0..amount {
            let value = part1[from - 1].pop().unwrap();
            part1[to - 1].push(value);
        }
    }
    let part1 = part1.iter().map(|s| s.last().unwrap()).collect::<String>();
    println!("Part 1: {}", part1);

    // Par 2
    let mut part2 = stacks.clone();
    for (amount, from, to) in moves {
        let mut values = vec![];
        for _ in 0..amount {
            values.push(part2[from - 1].pop().unwrap());
        }
        values.reverse();
        part2[to - 1].append(&mut values);
    }
    let part2 = part2.iter().map(|s| s.last().unwrap()).collect::<String>();
    println!("Part 2: {}", part2);
}
