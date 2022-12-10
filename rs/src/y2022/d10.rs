pub fn solve(input: String) {
    let mut history = vec![];
    let mut reg_x = 1;

    for line in input.lines() {
        if line == "noop" {
            history.push(reg_x);
        } else if let Some(arg) = line.strip_prefix("addx ") {
            history.push(reg_x);
            history.push(reg_x);
            reg_x += arg.parse::<i32>().unwrap();
        } else {
            panic!("Unknown instruction");
        }
    }

    let part1 = [20, 60, 100, 140, 180, 220]
        .into_iter()
        .map(|i| history[i - 1] * i as i32)
        .sum::<i32>();
    println!("Part 1: {part1}");

    println!("Part 2:");
    for chunk in history.chunks(40) {
        for (x, reg_x) in chunk.iter().enumerate() {
            let visible = (x as i32).abs_diff(*reg_x) <= 1;
            let pixel = if visible { '#' } else { '.' };
            print!("{pixel}");
        }
        println!();
    }
}
