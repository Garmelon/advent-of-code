use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Cell {
    Wall,
    Sand,
}

#[derive(Clone)]
struct Grid {
    grid: HashMap<(i32, i32), Cell>,
    max_y: i32,
}

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
        let max_y = *grid.keys().map(|(_, y)| y).max().unwrap();
        Self { grid, max_y }
    }

    /// Returns `true` if the sand emitted from the source came to rest. Returns
    /// `false` if the source is blocked or the sand fell into infinity.
    fn step(&mut self, source: (i32, i32)) -> bool {
        if self.grid.get(&source).is_some() {
            return false;
        }

        let (mut x, mut y) = source;
        while y <= self.max_y {
            if self.grid.get(&(x, y + 1)).is_none() {
                y += 1;
            } else if self.grid.get(&(x - 1, y + 1)).is_none() {
                x -= 1;
                y += 1;
            } else if self.grid.get(&(x + 1, y + 1)).is_none() {
                x += 1;
                y += 1;
            } else {
                self.grid.insert((x, y), Cell::Sand);
                return true;
            }
        }
        false
    }
}

pub fn solve(input: String) {
    let grid = Grid::parse(&input);

    let mut part1_grid = grid.clone();
    let mut part1 = 0;
    while part1_grid.step((500, 0)) {
        part1 += 1;
    }
    println!("Part 1: {part1}");

    let mut part2_grid = grid;
    part2_grid.max_y += 2;
    for x in 500 - part2_grid.max_y..=500 + part2_grid.max_y {
        part2_grid.grid.insert((x, part2_grid.max_y), Cell::Wall);
    }
    let mut part2 = 0;
    while part2_grid.step((500, 0)) {
        part2 += 1;
    }
    println!("Part 2: {part2}");
}
