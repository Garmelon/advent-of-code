use std::collections::HashSet;

#[derive(Clone, Copy)]
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
        // A beacon exactly dist away has 1 field, i. e. x±0
        // A beacon exactly dist-1 away has 3 fields, i. e. x±1
        // ...
        // A beacon exactly 0 away has 2dist+1 fields, i. e. x±dist
        //
        // Formula: Go from x-(dist-dy) to x+(dist-dy)
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

fn intersect_lines(tlbr: (i32, i32, i32, i32), trbl: (i32, i32, i32, i32)) -> Option<(i32, i32)> {
    // For tlbr lines, x1 - y1 = x2 - y2 = x - y = c.
    // For trbl lines, x1 + y1 = x2 + y2 = x + y = c.
    assert_eq!(tlbr.0 - tlbr.1, tlbr.2 - tlbr.3);
    assert_eq!(trbl.0 + trbl.1, trbl.2 + trbl.3);
    let c_tlbr = tlbr.0 - tlbr.1;
    let c_trbl = trbl.0 + trbl.1;

    // Find x, y such that
    // x - y = c_tlbr
    // x + y = c_trbl
    //
    // 2x = c_tlbr + c_trbl
    // y = c_trbl - x
    let two_x = c_tlbr + c_trbl;
    if two_x.rem_euclid(2) != 0 {
        return None; // Intersection not at integer coordinates
    }
    let x = two_x / 2;
    let y = c_trbl - x;
    assert_eq!(x - y, c_tlbr);
    assert_eq!(x + y, c_trbl);

    // Finally, check if the result is on both line segments. We only need to
    // check one dimension per line segment.
    let on_tlbr = tlbr.1 <= y && y <= tlbr.3;
    let on_trbl = trbl.1 <= y && y <= trbl.3;
    if on_tlbr && on_trbl {
        Some((x, y))
    } else {
        None
    }
}

fn covered_by(sensor: Sensor, pos: (i32, i32)) -> bool {
    let dist = (pos.0 - sensor.pos.0).abs() + (pos.1 - sensor.pos.1).abs();
    dist <= sensor.dist
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

    let mut lines_tlbr = vec![];
    let mut lines_trbl = vec![];
    for sensor in &sensors {
        let d = sensor.dist;
        let p = sensor.pos;
        lines_tlbr.push((p.0, p.1 - d - 1, p.0 + d + 1, p.1));
        lines_tlbr.push((p.0 - d - 1, p.1, p.0, p.1 + d + 1));
        lines_trbl.push((p.0, p.1 - d - 1, p.0 - d - 1, p.1));
        lines_trbl.push((p.0 + d + 1, p.1, p.0, p.1 + d + 1));
    }

    for tlbr in &lines_tlbr {
        for trbl in &lines_trbl {
            if let Some(intersect) = intersect_lines(*tlbr, *trbl) {
                let x_in_bounds = 0 <= intersect.0 && intersect.0 <= 4000000;
                let y_in_bounds = 0 <= intersect.1 && intersect.1 <= 4000000;
                if !x_in_bounds || !y_in_bounds {
                    continue;
                }
                let mut covered = false;
                for sensor in &sensors {
                    if covered_by(*sensor, intersect) {
                        covered = true;
                        break;
                    }
                }
                if covered {
                    continue;
                }
                // We found our candidate :)
                let tuning_frequency = intersect.0 as i64 * 4000000 + intersect.1 as i64;
                println!("Part 2: {tuning_frequency}");
                return;
            }
        }
    }
}
