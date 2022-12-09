use std::collections::HashSet;
use std::iter;

fn move_head(dir: &str, head: (i32, i32)) -> (i32, i32) {
    match dir {
        "L" => (head.0 - 1, head.1),
        "R" => (head.0 + 1, head.1),
        "D" => (head.0, head.1 - 1),
        "U" => (head.0, head.1 + 1),
        _ => head,
    }
}

fn move_tail(anchor: (i32, i32), tail: (i32, i32)) -> (i32, i32) {
    let mut dx = anchor.0 - tail.0;
    let mut dy = anchor.1 - tail.1;

    if dx.abs() == dy.abs() {
        // Move diagonally
        dx = dx.signum();
        dy = dy.signum();
    } else {
        // Move diagonally until either dx or dy is 0, then catch up along the
        // remaining axis.
        let diagonal_steps = dx.abs().min(dy.abs());
        dx = (dx - dx.signum() * diagonal_steps).signum();
        dy = (dy - dy.signum() * diagonal_steps).signum();
    }

    (anchor.0 - dx, anchor.1 - dy)
}

fn simulate_rope(input: &str, segments: usize) -> usize {
    let mut head = (0, 0);
    let mut tails = vec![(0, 0); segments - 1];
    let mut tail_trail = HashSet::new();
    for line in input.lines() {
        let (dir, amount) = line.split_once(' ').unwrap();
        let amount = amount.parse::<i32>().unwrap();
        for _ in 0..amount {
            head = move_head(dir, head);
            let mut anchor = head;
            for tail in &mut tails {
                *tail = move_tail(anchor, *tail);
                anchor = *tail;
            }
            tail_trail.insert(anchor);
        }
        // eprint_field(head, &tails, &tail_trail);
    }
    tail_trail.len()
}

pub fn solve(input: String) {
    println!("Part 1: {}", simulate_rope(&input, 2));
    println!("Part 2: {}", simulate_rope(&input, 10));
}

fn eprint_field(head: (i32, i32), tails: &[(i32, i32)], trail: &HashSet<(i32, i32)>) {
    let coords = iter::once((0, 0))
        .chain(iter::once(head))
        .chain(tails.iter().cloned())
        .chain(trail.iter().cloned())
        .collect::<Vec<_>>();
    let min_x = *coords.iter().map(|(x, _)| x).min().unwrap();
    let max_x = *coords.iter().map(|(x, _)| x).max().unwrap();
    let min_y = *coords.iter().map(|(_, y)| y).min().unwrap();
    let max_y = *coords.iter().map(|(_, y)| y).max().unwrap();
    let width = (max_x - min_x + 1) as usize;
    let height = (max_y - min_y + 1) as usize;
    let mut field = vec![vec!['.'; width]; height];
    let set = |field: &mut [Vec<char>], x: i32, y: i32, c: char| {
        field[(y - min_y) as usize][(x - min_x) as usize] = c;
    };
    for (x, y) in trail.iter() {
        set(&mut field, *x, *y, '#');
    }
    for (i, (x, y)) in tails.iter().enumerate().rev() {
        set(&mut field, *x, *y, (('1' as usize) + i) as u8 as char);
    }
    set(&mut field, head.0, head.1, 'H');
    set(&mut field, 0, 0, 's');
    for row in field.into_iter().rev() {
        eprintln!(
            "{}",
            row.iter()
                .map(|c| format!("{c}"))
                .collect::<Vec<_>>()
                .join("")
        );
    }
    eprintln!();
}
