use std::fs;

use day13::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day13.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must be present");
    let equations = parse(&input);

    let part_one_result = part_one(&equations);
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&equations);
    println!("Part 2 Solution = {part_two_result}");
}
