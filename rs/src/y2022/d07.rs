use std::collections::HashMap;

enum LsItem {
    File(usize, String),
    Dir(String),
}

enum Command {
    Cd(String),
    Ls(Vec<LsItem>),
}

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
        let mut result = self;
        for segment in path {
            result = result.as_dir().get_mut(segment).unwrap();
        }
        result
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

fn command_chunks(input: &str) -> Vec<(String, Vec<String>)> {
    let mut lines = input.lines().peekable();
    let mut chunks = vec![];
    while let Some(command) = lines.next() {
        assert!(command.starts_with("$ "));
        let mut items = vec![];
        while let Some(item) = lines.peek() {
            if item.starts_with("$ ") {
                break;
            }
            items.push(item.to_string());
            lines.next();
        }
        chunks.push((command.to_string(), items));
    }
    chunks
}

pub fn solve(input: String) {
    // Parse commands
    let commands = command_chunks(&input)
        .into_iter()
        .map(|(command, items)| {
            if let Some(name) = command.strip_prefix("$ cd ") {
                Command::Cd(name.to_string())
            } else {
                assert_eq!(command, "$ ls");
                let ls_items = items
                    .into_iter()
                    .map(|item| {
                        let (left, name) = item.split_once(' ').unwrap();
                        if left == "dir" {
                            LsItem::Dir(name.to_string())
                        } else {
                            LsItem::File(left.parse::<usize>().unwrap(), name.to_string())
                        }
                    })
                    .collect::<Vec<_>>();
                Command::Ls(ls_items)
            }
        })
        .collect::<Vec<_>>();

    // Build fs tree
    let mut fs = Fs::Dir(HashMap::new());
    let mut path = vec![];
    for command in commands {
        match command {
            Command::Cd(to) if to == "/" => path = vec![],
            Command::Cd(to) if to == ".." => _ = path.pop(), // Learned a new trick
            Command::Cd(to) => path.push(to),
            Command::Ls(items) => {
                for item in items {
                    let map = fs.at_path(&path).as_dir();
                    match item {
                        LsItem::File(size, name) => map.insert(name, Fs::File(size)),
                        LsItem::Dir(name) => map.insert(name, Fs::Dir(HashMap::new())),
                    };
                }
            }
        }
    }

    let dir_sizes = fs.dirs().into_iter().map(|d| d.size()).collect::<Vec<_>>();

    let part1 = dir_sizes.iter().filter(|s| **s <= 100000).sum::<usize>();
    println!("Part 1: {part1}");

    let total_available = 70000000;
    let required_for_update = 30000000;
    let unused = total_available - fs.size();
    let free_at_least = required_for_update - unused;
    let part2 = dir_sizes
        .into_iter()
        .filter(|s| *s >= free_at_least)
        .min()
        .unwrap();
    println!("Part 2: {part2}");
}
