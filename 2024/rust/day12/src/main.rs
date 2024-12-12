use std::fs;

use day12::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day12.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must exist");
    let plot = parse(&input);

    let part_one_result = part_one(&plot);
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&plot);
    println!("Part 2 Solution = {part_two_result}");
}
