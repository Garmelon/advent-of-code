use std::mem;
use std::str::Lines;

#[derive(Clone, Copy)]
enum Operation {
    Add,
    Mul,
}

impl Operation {
    fn eval(self, lhs: u32, rhs: u32) -> u32 {
        match self {
            Self::Add => lhs + rhs,
            Self::Mul => lhs * rhs,
        }
    }
}

struct Monkey {
    holds: Vec<u32>,

    // Left hand side is always "old"
    op: Operation,
    rhs: Option<u32>,

    div_by: u32,
    if_true: usize,
    if_false: usize,

    inspections: usize,
}

impl Monkey {
    fn parse(s: &str) -> Self {
        let mut lines = s.lines();
        lines.next();

        fn prefixed<'a>(lines: &'a mut Lines, prefix: &str) -> &'a str {
            lines.next().unwrap().strip_prefix(prefix).unwrap()
        }

        let holds = prefixed(&mut lines, "  Starting items: ")
            .split(", ")
            .map(|i| i.parse::<u32>().unwrap())
            .collect::<Vec<_>>();

        let (op, rhs) = prefixed(&mut lines, "  Operation: new = old ")
            .split_once(' ')
            .unwrap();
        let op = match op {
            "+" => Operation::Add,
            "*" => Operation::Mul,
            _ => panic!(),
        };
        let rhs = match rhs {
            "old" => None,
            _ => Some(rhs.parse().unwrap()),
        };

        let div_by = prefixed(&mut lines, "  Test: divisible by ")
            .parse()
            .unwrap();

        let if_true = prefixed(&mut lines, "    If true: throw to monkey ")
            .parse()
            .unwrap();

        let if_false = prefixed(&mut lines, "    If false: throw to monkey ")
            .parse()
            .unwrap();

        Self {
            holds,
            op,
            rhs,
            div_by,
            if_true,
            if_false,
            inspections: 0,
        }
    }

    fn take(&mut self) -> Self {
        Self {
            holds: mem::take(&mut self.holds),
            op: self.op,
            rhs: self.rhs,
            div_by: self.div_by,
            if_true: self.if_true,
            if_false: self.if_false,
            inspections: self.inspections,
        }
    }
}

fn round(monkeys: &mut Vec<Monkey>) {
    for i in 0..monkeys.len() {
        monkeys[i].inspections += monkeys[i].holds.len();
        let monkey = monkeys[i].take();
        for item in monkey.holds {
            let item = monkey.op.eval(item, monkey.rhs.unwrap_or(item)) / 3;
            let target = if item % monkey.div_by == 0 {
                monkey.if_true
            } else {
                monkey.if_false
            };
            monkeys[target].holds.push(item);
        }
    }
}

pub fn solve(input: String) {
    let mut monkeys = vec![];
    for monkey in input.trim().split("\n\n") {
        monkeys.push(Monkey::parse(monkey));
    }

    for _ in 0..20 {
        round(&mut monkeys);
    }

    let mut inspections = monkeys.iter().map(|m| m.inspections).collect::<Vec<_>>();
    inspections.sort_unstable();
    let part1 = inspections.into_iter().rev().take(2).product::<usize>();
    println!("Part 1: {part1}");
}
