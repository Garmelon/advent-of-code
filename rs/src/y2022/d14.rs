use std::ops::{Index, IndexMut};

struct Grid {
    width: usize,
    height: usize,
    cells: Vec<bool>,
}

impl Grid {
    fn new(width: usize, height: usize, initial: bool) -> Self {
        Self {
            width,
            height,
            cells: vec![initial; width * height],
        }
    }
}

impl Index<(i32, i32)> for Grid {
    type Output = bool;

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

fn parse_tuple(s: &str) -> (i32, i32) {
    let (x, y) = s.split_once(',').unwrap();
    (x.parse().unwrap(), y.parse().unwrap())
}

fn draw_lines(grid: &mut Grid, lines: Vec<Vec<(i32, i32)>>) {
    for line in lines {
        let mut line = line.into_iter();

        let mut cur = line.next().unwrap();
        grid[cur] = true;

        for next in line {
            let dir = ((next.0 - cur.0).signum(), (next.1 - cur.1).signum());
            assert!(dir.0 == 0 || dir.1 == 0);
            while cur != next {
                cur.0 += dir.0;
                cur.1 += dir.1;
                grid[cur] = true;
            }
        }
    }
}

fn draw_floor(grid: &mut Grid) {
    let y = grid.height as i32 - 1;
    for x in 0..grid.width {
        grid[(x as i32, y)] = true;
    }
}

/// The `path` is a glorified source. It is where the next unit of sand should
/// spawn. In particular, this function needs to ensure that when it returns,
/// the last element of the path should be the correct place to spawn the next
/// unit of sand. If the path is empty after this function returns, the original
/// source has been covered by sand.
///
/// The last row where this function will deposit sand is `max_y`.
///
/// This function will return `true` if it managed to deposit a unit of sand.
fn drop(grid: &mut Grid, path: &mut Vec<(i32, i32)>, max_y: i32) -> bool {
    let Some((mut x, mut y)) = path.last() else {
        return false;
    };

    loop {
        if y + 1 > max_y {
            return false;
        } else if !grid[(x, y + 1)] {
            y += 1;
            path.push((x, y));
        } else if !grid[(x - 1, y + 1)] {
            x -= 1;
            y += 1;
            path.push((x, y));
        } else if !grid[(x + 1, y + 1)] {
            x += 1;
            y += 1;
            path.push((x, y));
        } else {
            grid[(x, y)] = true;
            path.pop();
            return true;
        }
    }
}

pub fn solve(input: String) {
    let lines = input
        .lines()
        .map(|l| l.split(" -> ").map(parse_tuple).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut max_x = 500;
    let mut max_y = 0;
    for (x, y) in lines.iter().flat_map(|l| l.iter()) {
        max_x = max_x.max(*x);
        max_y = max_y.max(*y);
    }

    let mut grid = Grid::new((max_x + max_y) as usize, (max_y + 3) as usize, false);
    draw_lines(&mut grid, lines);
    draw_floor(&mut grid);

    let mut path = vec![(500, 0)];
    let mut drop_count = 0;
    while drop(&mut grid, &mut path, max_y) {
        drop_count += 1;
    }
    println!("Part 1: {drop_count}");

    while drop(&mut grid, &mut path, max_y + 2) {
        drop_count += 1;
    }
    println!("Part 2: {drop_count}");
}
