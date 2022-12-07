use std::collections::HashMap;

enum Fs {
    Dir(HashMap<String, Fs>),
    File(usize),
}

impl Fs {
    fn as_dir(&mut self) -> &mut HashMap<String, Self> {
        match self {
            Fs::Dir(map) => map,
            Fs::File(_) => panic!("Not a directory"),
        }
    }

    fn at_path(&mut self, path: &[String]) -> &mut Self {
        path.iter()
            .fold(self, |p, s| p.as_dir().get_mut(s).unwrap())
    }

    fn size(&self) -> usize {
        match self {
            Fs::Dir(map) => map.values().map(|v| v.size()).sum(),
            Fs::File(size) => *size,
        }
    }

    fn dirs(&self) -> Vec<&Self> {
        let mut result = vec![];
        if let Fs::Dir(map) = self {
            result.push(self);
            for value in map.values() {
                result.extend(value.dirs());
            }
        }
        result
    }
}

pub fn solve(input: String) {
    // Parse commands and build fs tree
    let mut fs = Fs::Dir(HashMap::new());
    let mut path = vec![];
    for line in input.lines() {
        if let Some(name) = line.strip_prefix("$ cd ") {
            match name {
                "/" => path.clear(),
                ".." => _ = path.pop(),
                _ => path.push(name.to_string()),
            }
        } else if line == "$ ls" {
        } else if let Some(name) = line.strip_prefix("dir ") {
            let dir = Fs::Dir(HashMap::new());
            fs.at_path(&path).as_dir().insert(name.to_string(), dir);
        } else {
            let (size, name) = line.split_once(' ').unwrap();
            let file = Fs::File(size.parse().unwrap());
            fs.at_path(&path).as_dir().insert(name.to_string(), file);
        }
    }

    let dir_sizes = fs.dirs().into_iter().map(|d| d.size()).collect::<Vec<_>>();

    let part1 = dir_sizes.iter().filter(|s| **s <= 100000).sum::<usize>();
    println!("Part 1: {part1}");

    // 70000000 available, 30000000 required for update: We can't go over 40000000.
    let min = fs.size() - 40000000;
    let part2 = dir_sizes.into_iter().filter(|s| *s >= min).min().unwrap();
    println!("Part 2: {part2}");
}
