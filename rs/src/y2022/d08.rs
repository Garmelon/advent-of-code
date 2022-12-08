struct Tree {
    height: u8,
    visible: bool,
    scenic: usize,
}

struct Grid {
    width: usize,
    height: usize,
    trees: Vec<Tree>,
}

impl Grid {
    fn parse(str: &str) -> Self {
        let width = str.lines().next().unwrap().len();
        let height = str.lines().count();
        let trees = str
            .lines()
            .flat_map(|l| l.chars())
            .map(|c| Tree {
                height: (c as u32 - '0' as u32) as u8,
                visible: false,
                scenic: 0,
            })
            .collect();
        Self {
            width,
            height,
            trees,
        }
    }

    fn at_mut(&mut self, x: usize, y: usize) -> Option<&mut Tree> {
        if x >= self.width || y >= self.height {
            None
        } else {
            Some(&mut self.trees[y * self.width + x])
        }
    }

    fn scan_visibility(&mut self, xy: impl Iterator<Item = (usize, usize)>) {
        let mut max_height = 0;
        for (x, y) in xy {
            let tree = self.at_mut(x, y).unwrap();
            if tree.height >= max_height {
                tree.visible = true;
                max_height = tree.height + 1;
            }
        }
    }

    fn viewing_distance(&mut self, height: u8, xy: impl Iterator<Item = (usize, usize)>) -> usize {
        let mut count = 0;
        for (x, y) in xy {
            count += 1;
            let new_height = self.at_mut(x, y).unwrap().height;
            if new_height >= height {
                break;
            }
        }
        count
    }
}

pub fn solve(input: String) {
    let mut trees = Grid::parse(&input);

    for y in 0..trees.height {
        trees.scan_visibility((0..trees.width).map(|x| (x, y)));
        trees.scan_visibility((0..trees.width).map(|x| (x, y)).rev());
    }
    for x in 0..trees.width {
        trees.scan_visibility((0..trees.height).map(|y| (x, y)));
        trees.scan_visibility((0..trees.height).map(|y| (x, y)).rev());
    }
    let part1 = trees.trees.iter().filter(|t| t.visible).count();
    println!("Part 1: {part1}");

    for y in 0..trees.height {
        for x in 0..trees.width {
            let height = trees.at_mut(x, y).unwrap().height;
            let up = trees.viewing_distance(height, (0..y).rev().map(|y| (x, y)));
            let down = trees.viewing_distance(height, (y..trees.height).skip(1).map(|y| (x, y)));
            let left = trees.viewing_distance(height, (0..x).rev().map(|x| (x, y)));
            let right = trees.viewing_distance(height, (x..trees.width).skip(1).map(|x| (x, y)));
            trees.at_mut(x, y).unwrap().scenic = up * down * left * right;
        }
    }
    let part2 = trees.trees.iter().map(|t| t.scenic).max().unwrap();
    println!("Part 2: {part2}");
}
