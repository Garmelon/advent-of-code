use std::collections::HashSet;

struct Sensor {
    pos: (i32, i32),
    beac: (i32, i32),
    dist: i32,
}

fn parse_coord(coord: &str) -> i32 {
    let coord = coord.replace(|c| "xy=,:".contains(c), "");
    coord.parse().unwrap()
}

pub fn solve(input: String) {
    let sensors = input
        .lines()
        .map(|l| {
            let parts = l.split_whitespace().collect::<Vec<_>>();
            let pos = (parse_coord(parts[2]), parse_coord(parts[3]));
            let beac = (parse_coord(parts[8]), parse_coord(parts[9]));
            let dist = (pos.0 - beac.0).abs() + (pos.1 - beac.1).abs();
            Sensor { pos, beac, dist }
        })
        .collect::<Vec<_>>();

    // A beacon exactly dist away has 1 field, i. e. x±0
    // A beacon exactly dist-1 away has 3 fields, i. e. x±1
    // ...
    // A beacon exactly 0 away has 2dist+1 fields, i. e. x±dist
    //
    // Formula: Go from x-(dist-dy) to x+(dist-dy)
    let line_at_y = 2000000;
    let mut part1 = HashSet::new();
    for (i, sensor) in sensors.iter().enumerate() {
        eprintln!("Sensor {}", i + 1);
        let dy = (line_at_y - sensor.pos.1).abs();
        if dy > sensor.dist {
            continue;
        }

        let dx = sensor.dist - dy;
        for x in (sensor.pos.0 - dx)..=(sensor.pos.0 + dx) {
            part1.insert(x);
        }
    }
    for sensor in &sensors {
        if sensor.beac.1 == line_at_y {
            part1.remove(&sensor.beac.0);
        }
    }
    println!("Part 1: {}", part1.len());
}
