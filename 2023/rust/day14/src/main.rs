use std::fs;

use day14::{parse, rock_roll_sum, transpose};

const INPUT_DIR: &str = "input";
fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let patterns = parse(&contents);

    let mut total_load = 0;
    for pattern in patterns {
        let transposed_pattern = transpose(&pattern);
        let sum = rock_roll_sum(&transposed_pattern);

        total_load += sum;
    }

    println!("Total load = {total_load}");
}
