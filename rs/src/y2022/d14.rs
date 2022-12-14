use std::collections::HashMap;
use std::thread;
use std::time::Duration;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Cell {
    Wall,
    Sand,
    Source,
}

struct Grid(HashMap<(i32, i32), Cell>);

impl Grid {
    fn parse(input: &str) -> Self {
        let mut grid = HashMap::new();
        for line in input.lines() {
            let mut corners = line.split(" -> ").map(|s| {
                let (x, y) = s.split_once(',').unwrap();
                (x.parse::<i32>().unwrap(), y.parse::<i32>().unwrap())
            });

            let mut cur = corners.next().unwrap();
            grid.insert(cur, Cell::Wall);

            for next in corners {
                let dir = ((next.0 - cur.0).signum(), (next.1 - cur.1).signum());
                assert!(dir.0 == 0 || dir.1 == 0);
                while cur != next {
                    cur.0 += dir.0;
                    cur.1 += dir.1;
                    grid.insert(cur, Cell::Wall);
                }
            }
        }
        Self(grid)
    }

    fn eprint(&self) {
        let min_x = *self.0.keys().map(|(x, _)| x).min().unwrap();
        let max_x = *self.0.keys().map(|(x, _)| x).max().unwrap();
        let min_y = *self.0.keys().map(|(_, y)| y).min().unwrap();
        let max_y = *self.0.keys().map(|(_, y)| y).max().unwrap();
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                match self.0.get(&(x, y)) {
                    None => eprint!(".."),
                    Some(Cell::Wall) => eprint!("##"),
                    Some(Cell::Sand) => eprint!("()"),
                    Some(Cell::Source) => eprint!("++"),
                }
            }
            eprintln!();
        }
    }

    /// Returns true if the sand emitted from the source came to rest.
    fn step(&mut self, source: (i32, i32)) -> bool {
        let (mut x, mut y) = source;
        let max_y = *self.0.keys().map(|(_, y)| y).max().unwrap();
        while y <= max_y {
            if self.0.get(&(x, y + 1)).is_none() {
                y += 1;
            } else if self.0.get(&(x - 1, y + 1)).is_none() {
                x -= 1;
                y += 1;
            } else if self.0.get(&(x + 1, y + 1)).is_none() {
                x += 1;
                y += 1;
            } else {
                self.0.insert((x, y), Cell::Sand);
                return true;
            }
        }
        false
    }
}

pub fn solve(input: String) {
    let mut grid = Grid::parse(&input);
    // grid.eprint();

    let mut part1 = 0;
    while grid.step((500, 0)) {
        part1 += 1;
        // eprintln!("\x1b[;H\x1b[J");
        // eprintln!();
        // grid.eprint();
    }
    println!("Part 1: {part1}");
}
