use std::fs;

use day15::{form_boxes, sum_focusing_power, sum_hashes};

const INPUT_DIR: &str = "input";
fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let sum = sum_hashes(&contents);

    println!("Sum of hashes = {sum}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let boxes = form_boxes(&contents);
    let focussing_power = sum_focusing_power(&boxes);

    println!("Total focussing power = {focussing_power}");
}
