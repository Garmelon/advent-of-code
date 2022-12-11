use std::mem;
use std::str::Lines;

#[derive(Debug, Clone, Copy)]
enum Operation {
    Add,
    Mul,
}

impl Operation {
    fn eval(self, lhs: u64, rhs: u64) -> u64 {
        match self {
            Self::Add => lhs + rhs,
            Self::Mul => lhs * rhs,
        }
    }
}

#[derive(Clone)]
struct Monkey {
    holds: Vec<u64>,

    // Left hand side is always "old"
    op: Operation,
    rhs: Option<u64>,

    div_by: u64,
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
            .map(|i| i.parse::<u64>().unwrap())
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

fn round(monkeys: &mut Vec<Monkey>, reduce_worry: bool, modulo: u64) {
    for i in 0..monkeys.len() {
        monkeys[i].inspections += monkeys[i].holds.len();
        let monkey = monkeys[i].take();
        for item in monkey.holds {
            let item = monkey.op.eval(item, monkey.rhs.unwrap_or(item));
            let item = if reduce_worry { item / 3 } else { item };
            let item = item % modulo;
            let target = if item % monkey.div_by == 0 {
                monkey.if_true
            } else {
                monkey.if_false
            };
            monkeys[target].holds.push(item);
        }
    }
}

fn monkey_business(monkeys: &[Monkey]) -> usize {
    let mut inspections = monkeys.iter().map(|m| m.inspections).collect::<Vec<_>>();
    inspections.sort_unstable();
    inspections.into_iter().rev().take(2).product::<usize>()
}

pub fn solve(input: String) {
    let mut monkeys = vec![];
    for monkey in input.trim().split("\n\n") {
        monkeys.push(Monkey::parse(monkey));
    }

    let common_multiple = monkeys.iter().map(|m| m.div_by).product::<u64>();
    eprintln!("Common multiple: {common_multiple}");

    let mut part1 = monkeys.clone();
    for _ in 0..20 {
        round(&mut part1, true, common_multiple);
    }
    println!("Part 1: {}", monkey_business(&part1));

    let mut part2 = monkeys.clone();
    for _ in 0..10000 {
        round(&mut part2, false, common_multiple);
    }
    println!("Part 2: {}", monkey_business(&part2));
}
