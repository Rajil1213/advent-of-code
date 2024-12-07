use std::fs;

use day07::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day07.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("must be able to read input file");
    let entries = parse(&input);

    let part_one_result = part_one(&entries);
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&entries);
    println!("Part 2 Solution = {part_two_result}");
}
