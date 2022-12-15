use std::mem;
use std::str::Lines;

#[derive(Debug, Clone, Copy, Default)]
enum Operation {
    Add(u64),
    Mul(u64),
    #[default]
    Square,
}

impl Operation {
    fn eval(self, old: u64) -> u64 {
        match self {
            Self::Add(lit) => old + lit,
            Self::Mul(lit) => old * lit,
            Self::Square => old * old,
        }
    }
}

#[derive(Clone, Default)]
struct Monkey {
    holds: Vec<u64>,

    op: Operation,

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
        let op = match (op, rhs) {
            ("+", _) => Operation::Add(rhs.parse().unwrap()),
            ("*", "old") => Operation::Square,
            ("*", _) => Operation::Mul(rhs.parse().unwrap()),
            _ => panic!(),
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
            div_by,
            if_true,
            if_false,
            inspections: 0,
        }
    }

    fn fill_into(&mut self, into: &mut Self) {
        mem::swap(&mut self.holds, &mut into.holds);
        self.holds.clear();
        into.op = self.op;
        into.div_by = self.div_by;
        into.if_true = self.if_true;
        into.if_false = self.if_false;
        into.inspections = self.inspections;
    }
}

#[inline]
fn round(monkeys: &mut Vec<Monkey>, swapmonkey: &mut Monkey, reduce_worry: bool, modulo: u64) {
    for i in 0..monkeys.len() {
        monkeys[i].inspections += monkeys[i].holds.len();
        monkeys[i].fill_into(swapmonkey);
        for item in &swapmonkey.holds {
            let item = swapmonkey.op.eval(*item);
            let item = if reduce_worry { item / 3 } else { item };
            let item = item % modulo;
            let target = if item % swapmonkey.div_by == 0 {
                swapmonkey.if_true
            } else {
                swapmonkey.if_false
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
    let mut swapmonkey = Monkey::default();

    let common_multiple = monkeys.iter().map(|m| m.div_by).product::<u64>();
    eprintln!("Common multiple: {common_multiple}");

    let mut part1 = monkeys.clone();
    for _ in 0..20 {
        round(&mut part1, &mut swapmonkey, true, common_multiple);
    }
    println!("Part 1: {}", monkey_business(&part1));

    let mut part2 = monkeys.clone();
    for _ in 0..10000 {
        round(&mut part2, &mut swapmonkey, false, common_multiple);
    }
    println!("Part 2: {}", monkey_business(&part2));
}
