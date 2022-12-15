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

fn line_ranges(sensors: &[Sensor], y: i32) -> Vec<(i32, i32)> {
    let mut raw_ranges = vec![];
    for sensor in sensors {
        let dy = (y - sensor.pos.1).abs();
        if dy > sensor.dist {
            continue;
        }
        let dx = sensor.dist - dy;
        raw_ranges.push((sensor.pos.0 - dx, sensor.pos.0 + dx));
    }
    raw_ranges.sort();

    let mut ranges = vec![];
    let mut raw_ranges = raw_ranges.into_iter();
    if let Some(mut cur) = raw_ranges.next() {
        for range in raw_ranges {
            if cur.0 <= range.0 && range.0 <= cur.1 {
                cur.1 = cur.1.max(range.1);
            } else {
                ranges.push(cur);
                cur = range;
            }
        }
        ranges.push(cur);
    }
    ranges
}

pub fn solve(input: String) {
    let sensors = input
        .lines()
        .map(|l| {
            // A beacon exactly dist away has 1 field, i. e. x±0
            // A beacon exactly dist-1 away has 3 fields, i. e. x±1
            // ...
            // A beacon exactly 0 away has 2dist+1 fields, i. e. x±dist
            //
            // Formula: Go from x-(dist-dy) to x+(dist-dy)
            let parts = l.split_whitespace().collect::<Vec<_>>();
            let pos = (parse_coord(parts[2]), parse_coord(parts[3]));
            let beac = (parse_coord(parts[8]), parse_coord(parts[9]));
            let dist = (pos.0 - beac.0).abs() + (pos.1 - beac.1).abs();
            Sensor { pos, beac, dist }
        })
        .collect::<Vec<_>>();

    let line_at_y = 2000000;
    let part1_ranges = line_ranges(&sensors, line_at_y);
    let part1_beacons = sensors
        .iter()
        .filter(|s| s.beac.1 == line_at_y)
        .map(|s| s.beac.0)
        .collect::<HashSet<_>>();
    let mut part1 = 0;
    for range in part1_ranges {
        part1 += range.1 - range.0 + 1;
        for beacon in &part1_beacons {
            if range.0 <= *beacon && *beacon <= range.1 {
                part1 -= 1;
            }
        }
    }
    println!("Part 1: {part1}");

    for y in 0..=4000000 {
        let mut ranges = line_ranges(&sensors, y);
        ranges.retain(|(s, e)| *s <= 4000000 && 0 <= *e);
        if ranges.len() != 1 {
            // Found our beacon!
            assert_eq!(ranges.len(), 2);
            let x = ranges[0].1 + 1;
            let tuning_frequency = x as i64 * 4000000 + y as i64;
            println!("Part 2: {tuning_frequency}");
            break;
        }
    }
}
