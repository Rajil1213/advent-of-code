use std::fs;

use day15::sum_hashes;

const INPUT_DIR: &str = "input";
fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let sum = sum_hashes(&contents);

    println!("Sum of hashes = {sum}");
}
