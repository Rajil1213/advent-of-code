use std::fs;

use day01::get_calibration_value;

const INPUT_LOCATION: &str = "input/values.txt";

fn main() {
    let full_input = fs::read_to_string(INPUT_LOCATION).expect("input file must be readable");

    println!("Sum of calibration values = {}", part_01(&full_input));
}

fn part_01(full_input: &str) -> u64 {
    let lines = full_input.lines();

    let mut sum: u64 = 0;
    for line in lines {
        sum += get_calibration_value(line);
    }

    sum
}
