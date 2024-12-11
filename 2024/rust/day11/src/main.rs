use std::fs;

use day11::{parse, part_one};

const INPUT_FILE: &str = "input/day11.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("must be able to read input file");
    let stones = parse(&input);

    let num_blinks = 25;
    let part_one_result = part_one(&stones, num_blinks);
    println!("Part 1 Solution: {part_one_result}");

    let num_blinks = 75;
    let part_two_result = part_one(&stones, num_blinks);
    println!("Part 2 Solution: {part_two_result}");
}
