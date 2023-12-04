use std::fs;

use day04::{calculate_points, parse};

const INPUT_DIR: &str = "input";

fn main() {
    part_01(&format!("{INPUT_DIR}/part_one_input.txt"));
}

fn part_01(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut sum = 0;
    for line in contents.lines() {
        let (winning, inhand) = parse(line);
        let points = calculate_points(winning, inhand);

        sum += points;
    }

    println!("Total points = {sum}");
}
