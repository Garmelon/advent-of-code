use std::collections::HashMap;
use std::mem;

fn parse_valve(line: &str) -> (&str, u32, Vec<&str>) {
    let (name, rest) = line
        .strip_prefix("Valve ")
        .unwrap()
        .split_once(" has flow rate=")
        .unwrap();
    let (rate, rest) = rest.split_once("; ").unwrap();
    let next = rest
        .strip_prefix("tunnel leads to valve ")
        .or_else(|| rest.strip_prefix("tunnels lead to valves "))
        .unwrap()
        .split(", ")
        .collect::<Vec<_>>();
    (name, rate.parse().unwrap(), next)
}
#[derive(Debug)]
struct Valve {
    rate: u32,
    next: Vec<usize>,
}

fn prepare_valves<'a>(
    mut valves: Vec<(&'a str, u32, Vec<&str>)>,
) -> (HashMap<&'a str, usize>, Vec<Valve>) {
    // All valves with a nonzero rate should come before the ones with a rate of
    // zero. This will help with our valve bitset later.
    valves.sort_by_key(|(_, rate, _)| *rate);
    valves.reverse();

    let names = valves
        .iter()
        .enumerate()
        .map(|(i, (n, _, _))| (*n, i))
        .collect::<HashMap<_, _>>();

    let valves = valves
        .into_iter()
        .map(|(_, rate, next)| {
            let next = next.into_iter().map(|n| names[n]).collect();
            Valve { rate, next }
        })
        .collect::<Vec<_>>();

    (names, valves)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct OpenSet(u64);

impl OpenSet {
    const ALL_CLOSED: Self = Self(0);

    /// The powerset of the set of all open valves.
    fn powerset(valves: &[Valve]) -> Vec<Self> {
        assert!(valves.len() <= 63);
        let first_zero_rate = valves
            .iter()
            .position(|v| v.rate == 0)
            .unwrap_or(valves.len());
        let until = 2_u64.pow(first_zero_rate as u32);
        (0..until).map(Self).collect::<Vec<_>>()
    }

    fn is_open(&self, valve_id: usize) -> bool {
        assert!(valve_id < 63);
        self.0 & (1 << valve_id) != 0
    }

    fn open(&self, valve_id: usize) -> Self {
        assert!(valve_id < 63);
        Self(self.0 | (1 << valve_id))
    }
}

#[derive(Debug)]
struct Dp1 {
    powers: usize,
    elems: Vec<u32>,
}

impl Dp1 {
    fn new(valves: &[Valve], powerset: &[OpenSet]) -> Self {
        let valves = valves.len();
        let powers = powerset.len();
        let elems = vec![0; valves * powers];
        Self { powers, elems }
    }

    fn get(&self, valve_id: usize, open: OpenSet) -> u32 {
        self.elems[self.powers * valve_id + open.0 as usize]
    }

    fn set(&mut self, valve_id: usize, open: OpenSet, score: u32) {
        self.elems[self.powers * valve_id + open.0 as usize] = score;
    }

    fn clear(&mut self) {
        self.elems.fill(0);
    }
}

fn solve_part_1(names: &HashMap<&str, usize>, valves: &[Valve], powerset: &[OpenSet]) -> u32 {
    // DP state consists of:
    // - The current minute
    // - The current valve
    // - The current set of open/closed valves

    let mut prev = Dp1::new(valves, powerset);
    let mut curr = Dp1::new(valves, powerset);

    // Given valve v in minute t with set s, you can either...
    // - Go to another neighbouring valve v', points: points[v', t-1, s]
    // - If v not in s: Open current valve, points: points[v, t-1, s+{v}] + t

    for minute in 1..=30 {
        eprintln!("Minute {minute}");

        mem::swap(&mut curr, &mut prev);
        curr.clear();

        for (valve_id, valve) in valves.iter().enumerate() {
            for open in powerset {
                let mut score = valve
                    .next
                    .iter()
                    .map(|next_id| prev.get(*next_id, *open))
                    .max()
                    .unwrap_or(0);

                if valve.rate > 0 && !open.is_open(valve_id) {
                    let room_but_valve_open = prev.get(valve_id, open.open(valve_id));
                    let pressure_until_end = (minute - 1) * valve.rate;
                    score = score.max(room_but_valve_open + pressure_until_end);
                }

                curr.set(valve_id, *open, score);
            }
        }
    }

    curr.get(names["AA"], OpenSet::ALL_CLOSED)
}

pub fn solve(input: String) {
    let valves = input.lines().map(parse_valve).collect::<Vec<_>>();

    let (names, valves) = prepare_valves(valves);
    let powerset = OpenSet::powerset(&valves);
    eprintln!("Powerset has size {}", powerset.len());

    let part1 = solve_part_1(&names, &valves, &powerset);
    println!("Part 1: {part1}");
}
