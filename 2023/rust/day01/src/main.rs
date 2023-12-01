use std::{fs, u64};

use day01::{get_calibration_value, get_real_calibration_value};

const INPUT_DIR: &str = "input";

fn main() {
    part_01();
    part_02();
}

fn part_01() {
    let full_input = fs::read_to_string(format!("{INPUT_DIR}/values.txt"))
        .expect("input file values.txt must be readable");

    let lines = full_input.lines();

    let mut sum: u64 = 0;
    for line in lines {
        sum += get_calibration_value(line);
    }

    println!("Sum of calibration values = {}", sum);
}

fn part_02() {
    let full_input = fs::read_to_string(format!("{INPUT_DIR}/real_values.txt"))
        .expect("input file: real_values.txt must be readable");

    let lines = full_input.lines();

    let mut sum: u64 = 0;
    for line in lines {
        sum += get_real_calibration_value(line);
    }

    println!("Sum of real calibration values = {}", sum);
}
