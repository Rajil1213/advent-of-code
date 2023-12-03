use std::fs;

use day03::compute_parts_sum;

const INPUT_DIR: &str = "input";

fn main() {
    part_01(&format!("{INPUT_DIR}/part_one_visual.txt"));
    // part_01(&format!("{INPUT_DIR}/part_one_test.txt"));
}

fn part_01(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let matrix = contents.lines().collect::<Vec<&str>>();

    let sum = compute_parts_sum(matrix);

    println!("Sum of parts = {sum}");
}
