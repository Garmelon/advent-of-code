struct Tree {
    height: u8,
    visible: bool,
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
}
