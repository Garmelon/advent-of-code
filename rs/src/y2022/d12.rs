use std::collections::HashSet;
use std::ops::{Index, IndexMut};

struct Grid {
    width: usize,
    height: usize,
    cells: Vec<u32>,
}

impl Grid {
    fn new(width: usize, height: usize, initial: u32) -> Self {
        Self {
            width,
            height,
            cells: vec![initial; width * height],
        }
    }

    fn contains(&self, pos: (i32, i32)) -> bool {
        let x_in_bounds = 0 <= pos.0 && (pos.0 as usize) < self.width;
        let y_in_bounds = 0 <= pos.1 && (pos.1 as usize) < self.height;
        x_in_bounds && y_in_bounds
    }
}

impl Index<(i32, i32)> for Grid {
    type Output = u32;

    fn index(&self, index: (i32, i32)) -> &Self::Output {
        assert!(index.0 >= 0);
        assert!(index.1 >= 0);
        let (x, y) = (index.0 as usize, index.1 as usize);
        assert!(x < self.width);
        assert!(y < self.height);
        self.cells.index(y * self.width + x)
    }
}

impl IndexMut<(i32, i32)> for Grid {
    fn index_mut(&mut self, index: (i32, i32)) -> &mut Self::Output {
        assert!(index.0 >= 0);
        assert!(index.1 >= 0);
        let (x, y) = (index.0 as usize, index.1 as usize);
        assert!(x < self.width);
        assert!(y < self.height);
        self.cells.index_mut(y * self.width + x)
    }
}

fn backwards_neighbours(grid: &Grid, (x, y): (i32, i32)) -> impl Iterator<Item = (i32, i32)> + '_ {
    // One step down or arbitrarily many steps up are allowed
    let height = grid[(x, y)];
    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        .into_iter()
        .filter(move |n| grid.contains(*n) && grid[*n].saturating_add(1) >= height)
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

            for neighbour in backwards_neighbours(grid, pos) {
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
    let width = input.lines().next().unwrap().len();
    let height = input.lines().count();
    let mut grid = Grid::new(width, height, 0);

    let mut start = (-1, -1);
    let mut end = (-1, -1);
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
            grid[pos] = height;
        }
    }

    let part1 = bfs(&grid, end, |pos| pos == start);
    println!("Part 1: {part1}");

    let part2 = bfs(&grid, end, |pos| grid[pos] == 0);
    println!("Part 2: {part2}");
}
