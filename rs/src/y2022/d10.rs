#[derive(Clone, Copy)]
struct State {
    x: i32,
}

struct Run {
    history: Vec<State>,
    now: State,
}

impl Run {
    fn new() -> Self {
        Self {
            history: vec![],
            now: State { x: 1 },
        }
    }

    fn noop(&mut self) {
        self.history.push(self.now);
    }

    fn addx(&mut self, arg: i32) {
        self.noop();
        self.noop();
        self.now.x += arg;
    }
}

pub fn solve(input: String) {
    let mut run = Run::new();
    for line in input.lines() {
        if line == "noop" {
            run.noop();
        } else if let Some(arg) = line.strip_prefix("addx ") {
            run.addx(arg.parse().unwrap());
        } else {
            panic!("Unknown instruction");
        }
    }

    let part1 = [20, 60, 100, 140, 180, 220]
        .into_iter()
        .map(|i| run.history[i - 1].x * i as i32)
        .sum::<i32>();
    println!("Part 1: {part1}");
}
