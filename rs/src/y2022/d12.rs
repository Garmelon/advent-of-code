use std::collections::{HashMap, HashSet};

type Grid = HashMap<(i32, i32), u32>;

fn neighbours(grid: &Grid, (x, y): (i32, i32)) -> impl Iterator<Item = (i32, i32)> + '_ {
    let height = grid.get(&(x, y)).cloned().unwrap_or(u32::MAX);
    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        .into_iter()
        .filter(move |n| {
            let height2 = grid.get(n).cloned().unwrap_or(u32::MAX);
            // One step down or arbitrarily many steps up are allowed
            height2.saturating_add(1) >= height
        })
}

fn bfs(grid: &Grid, start: (i32, i32), until: impl Fn((i32, i32)) -> bool) -> usize {
    let mut visited = HashSet::new();
    let mut queue = HashSet::new();
    let mut steps = 0;

    queue.insert(start);

    loop {
        let mut new_queue = HashSet::new();
        for pos in queue {
            if until(pos) {
                return steps;
            }

            for neighbour in neighbours(grid, pos) {
                if !visited.contains(&neighbour) {
                    visited.insert(neighbour);
                    new_queue.insert(neighbour);
                }
            }
        }
        queue = new_queue;
        steps += 1;
    }
}

pub fn solve(input: String) {
    let mut start = (-1, -1);
    let mut end = (-1, -1);
    let mut grid = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let pos = (x as i32, y as i32);
            match c {
                'S' => start = pos,
                'E' => end = pos,
                _ => {}
            }

            let height = match c {
                'S' => 'a',
                'E' => 'z',
                _ => c,
            };
            let height = height as u32 - 'a' as u32;
            grid.insert(pos, height);
        }
    }

    let part1 = bfs(&grid, end, |pos| pos == start);
    println!("Part 1: {part1}");

    let part2 = bfs(&grid, end, |pos| grid.get(&pos) == Some(&0));
    println!("Part 2: {part2}");
}
