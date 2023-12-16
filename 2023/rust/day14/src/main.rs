use std::fs;

use day14::{
    calculate_load_after_cycles, cycle_length, parse, rock_roll_sum, transpose, TOTAL_CYCLES,
};

const INPUT_DIR: &str = "input";
fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
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

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let patterns = parse(&contents);

    let (min_patterns, offset) = cycle_length(&patterns[0]);
    let load = calculate_load_after_cycles(TOTAL_CYCLES, &min_patterns, offset);

    println!("Total load after {TOTAL_CYCLES} cycles = {load}");
}
