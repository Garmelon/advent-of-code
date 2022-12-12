use std::collections::BinaryHeap;

#[derive(Debug)]
struct Grid<T> {
    width: usize,
    height: usize,
    cells: Vec<T>,
}

impl<T> Grid<T> {
    fn new(width: usize, height: usize, initial_value: T) -> Self
    where
        T: Clone,
    {
        Self {
            width,
            height,
            cells: vec![initial_value; width * height],
        }
    }

    fn index(&self, x: usize, y: usize) -> Option<usize> {
        if x >= self.width || y >= self.height {
            None
        } else {
            Some(y * self.width + x)
        }
    }

    fn at(&self, x: usize, y: usize) -> Option<&T> {
        Some(&self.cells[self.index(x, y)?])
    }

    fn at_mut(&mut self, x: usize, y: usize) -> Option<&mut T> {
        let index = self.index(x, y)?;
        Some(&mut self.cells[index])
    }

    fn indexi(&self, x: i32, y: i32) -> Option<usize> {
        let width = self.width as i32;
        let height = self.height as i32;
        if x < 0 || x >= width || y < 0 || y >= height {
            None
        } else {
            Some((y * width + x) as usize)
        }
    }

    fn ati(&self, x: i32, y: i32) -> Option<&T> {
        Some(&self.cells[self.indexi(x, y)?])
    }

    fn ati_mut(&mut self, x: i32, y: i32) -> Option<&mut T> {
        let index = self.indexi(x, y)?;
        Some(&mut self.cells[index])
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct Candidate {
    cost: usize,
    pos: (i32, i32),
    prev: (i32, i32),
}

impl Ord for Candidate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (other.cost)
            .cmp(&self.cost)
            .then_with(|| self.pos.cmp(&other.pos))
            .then_with(|| self.prev.cmp(&other.prev))
    }
}

impl PartialOrd for Candidate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Copy)]
struct Step {
    cost: usize,
    prev: (i32, i32),
}

impl Step {
    fn empty() -> Self {
        Self {
            cost: usize::MAX,
            prev: (-1, -1),
        }
    }
}

fn neighbours(grid: &Grid<u32>, pos: (i32, i32)) -> Vec<(i32, i32)> {
    let h = *grid.ati(pos.0, pos.1).unwrap();

    let mut result = vec![];
    let potential_neighbours = [
        (pos.0 - 1, pos.1),
        (pos.0 + 1, pos.1),
        (pos.0, pos.1 - 1),
        (pos.0, pos.1 + 1),
    ];
    for npos in potential_neighbours {
        if let Some(nh) = grid.ati(npos.0, npos.1) {
            if *nh <= h + 1 {
                result.push(npos);
            }
        }
    }
    result
}

fn dijkstra(grid: &Grid<u32>, start: (i32, i32), end: (i32, i32)) -> Grid<Step> {
    let mut steps = Grid::new(grid.width, grid.height, Step::empty());

    let mut heap = BinaryHeap::new();
    heap.push(Candidate {
        cost: 0,
        pos: start,
        prev: start,
    });

    while let Some(Candidate { cost, pos, prev }) = heap.pop() {
        let mut current = steps.ati_mut(pos.0, pos.1).unwrap();
        if pos == end {
            current.cost = cost;
            current.prev = prev;
            break;
        } else if cost < current.cost {
            current.cost = cost;
            current.prev = prev;

            for neighbour in neighbours(grid, pos) {
                heap.push(Candidate {
                    cost: cost + 1,
                    pos: neighbour,
                    prev: pos,
                })
            }
        }
    }

    steps
}

fn path_length(steps: &Grid<Step>, start: (i32, i32), end: (i32, i32)) -> usize {
    let mut pos = end;
    let mut length = 0;
    while pos != start {
        if let Some(step) = steps.ati(pos.0, pos.1) {
            pos = step.prev;
            length += 1;
        } else {
            return usize::MAX;
        }
    }
    length
}

pub fn solve(input: String) {
    let width = input.lines().next().unwrap().len();
    let height = input.lines().count();

    let mut start = (0, 0);
    let mut end = (0, 0);
    let mut grid = Grid::new(width, height, 0);
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let c = match c {
                'S' => {
                    start = (x, y);
                    'a'
                }
                'E' => {
                    end = (x, y);
                    'z'
                }
                _ => c,
            };
            *grid.at_mut(x, y).unwrap() = c as u32 - 'a' as u32;
        }
    }

    let starti = (start.0 as i32, start.1 as i32);
    let endi = (end.0 as i32, end.1 as i32);
    let steps = dijkstra(&grid, starti, endi);
    let part1 = path_length(&steps, starti, endi);
    println!("Part 1: {part1}");

    let mut part2 = usize::MAX;
    for y in 0..grid.height {
        for x in 0..grid.width {
            if *grid.at(x, y).unwrap() == 0 {
                let starti = (x as i32, y as i32);
                let endi = (end.0 as i32, end.1 as i32);
                let steps = dijkstra(&grid, starti, endi);
                let length = path_length(&steps, starti, endi);
                part2 = part2.min(length);
            }
        }
    }
    println!("Part 2: {part2}");
}
