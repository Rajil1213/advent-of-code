use std::fs;

use day03::{compute_parts_sum, compute_total_gear_ratio};

const INPUT_DIR: &str = "input";

fn main() {
    part_01(&format!("{INPUT_DIR}/part_one_visual.txt"));
    part_02(&format!("{INPUT_DIR}/part_two_visual.txt"));
}

fn part_01(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let matrix = contents.lines().collect::<Vec<&str>>();

    let sum = compute_parts_sum(matrix);

    println!("Sum of parts = {sum}");
}

fn part_02(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let matrix = contents.lines().collect::<Vec<&str>>();

    let total = compute_total_gear_ratio(matrix);

    println!("Total gear ratio = {total}");
}
