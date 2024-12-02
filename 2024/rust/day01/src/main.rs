use std::fs;

use day01::{parse_input, part_one, part_two};

const INPUT_DIR: &str = "input";

fn main() {
    let input = fs::read_to_string(format!("{INPUT_DIR}/day1.txt")).unwrap();
    let parsed_input = parse_input(input);

    let part_one_result = part_one(parsed_input.clone());
    println!("Part 1: {part_one_result}");

    let part_two_result = part_two(parsed_input);
    println!("Part 2: {part_two_result}");
}
