use std::collections::HashSet;

fn simulate_rope(input: &str, segments: usize) -> usize {
    let mut head = (0_i32, 0_i32);
    let mut tails = vec![(0, 0); segments - 1];
    let mut trail = HashSet::new();
    for line in input.lines() {
        let (dir, amount) = line.split_once(' ').unwrap();
        let amount = amount.parse::<i32>().unwrap();
        for _ in 0..amount {
            match dir {
                "L" => head.0 -= 1,
                "R" => head.0 += 1,
                "D" => head.1 -= 1,
                "U" => head.1 += 1,
                _ => panic!(),
            }
            let mut anchor = head;
            for tail in &mut tails {
                let (dx, dy) = (anchor.0 - tail.0, anchor.1 - tail.1);
                if dx.abs() > 1 || dy.abs() > 1 {
                    tail.0 += dx.signum();
                    tail.1 += dy.signum();
                }
                anchor = *tail;
            }
            trail.insert(anchor);
        }
    }
    trail.len()
}

pub fn solve(input: String) {
    println!("Part 1: {}", simulate_rope(&input, 2));
    println!("Part 2: {}", simulate_rope(&input, 10));
}
