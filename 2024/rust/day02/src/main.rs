use std::fs;

use day02::{parse, part_one, part_two};

const INPUT: &str = "input/day2.txt";

fn main() {
    let input = fs::read_to_string(INPUT).unwrap();
    let input = parse(input);

    let part_one_result = part_one(input.clone());
    println!("Part one solution: {part_one_result}");

    let part_two_result = part_two(input);
    println!("Part two solution: {part_two_result}");
}
