use std::collections::HashMap;

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

pub fn solve(input: String) {
    let valves = input.lines().map(parse_valve).collect::<Vec<_>>();

    let (names, valves) = prepare_valves(valves);
    let powerset = OpenSet::powerset(&valves);
    eprintln!("Powerset has size {}", powerset.len());

    // DP state consists of:
    // - The current minute
    // - The current valve
    // - The current set of open/closed valves

    let mut dp = HashMap::new();

    // Initialize for end state
    eprintln!("Minute 0");
    for valve_id in 0..valves.len() {
        for open in &powerset {
            dp.insert((0_u32, valve_id, *open), 0);
        }
    }

    // Given valve v in minute t with set s, you can either...
    // - Go to another neighbouring valve v', points: points[v', t-1, s]
    // - If v not in s: Open current valve, points: points[v, t-1, s+{v}] + t

    for minute in 1..=30 {
        eprintln!("Minute {minute}");
        for (valve_id, valve) in valves.iter().enumerate() {
            for open in &powerset {
                let mut score = valve
                    .next
                    .iter()
                    .map(|next_id| dp[&(minute - 1, *next_id, *open)])
                    .max()
                    .unwrap_or(0);

                if valve.rate > 0 && !open.is_open(valve_id) {
                    let room_but_valve_open = dp[&(minute - 1, valve_id, open.open(valve_id))];
                    let pressure_until_end = (minute - 1) * valve.rate;
                    score = score.max(room_but_valve_open + pressure_until_end);
                }

                dp.insert((minute, valve_id, *open), score);
            }
        }
    }

    let part1 = dp[&(30, names["AA"], OpenSet::ALL_CLOSED)];
    println!("Part 1: {part1}");
}
