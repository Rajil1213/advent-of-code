use std::fs;

use day19::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day19.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must be present");
    let (available_towels, target_designs) = parse(&input);

    let part_one_result = part_one(&available_towels, &target_designs);
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&available_towels, &target_designs);
    println!("Part 2 Solution = {part_two_result}");
}
