use std::fs;

use day13::{find_horizontal_reflection_line, find_vertical_reflection_line, parse_contents};

const INPUT_DIR: &str = "input";
fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let patterns = parse_contents(&contents);

    let mut sum = 0;

    for (i, pattern) in patterns.iter().enumerate() {
        let reflection_line = find_horizontal_reflection_line(pattern, false);
        if reflection_line.is_some() {
            sum += 100 * reflection_line.unwrap();
            continue;
        }

        let reflection_line = find_vertical_reflection_line(pattern, false);

        if reflection_line.is_some() {
            sum += reflection_line.unwrap();
            continue;
        }

        panic!("no reflection line found for pattern {i}");
    }

    println!("Final summary = {sum}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let patterns = parse_contents(&contents);

    let mut sum = 0;

    for (i, pattern) in patterns.iter().enumerate() {
        let reflection_line = find_horizontal_reflection_line(pattern, true);
        if reflection_line.is_some() {
            sum += 100 * reflection_line.unwrap();
            continue;
        }

        let reflection_line = find_vertical_reflection_line(pattern, true);

        if reflection_line.is_some() {
            sum += reflection_line.unwrap();
            continue;
        }

        panic!("no reflection line found for pattern {i}");
    }

    println!("Final summary after smudge fix = {sum}");
}
