use std::fs;

use day14::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day14.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must be present");

    let data = parse(&input);

    let part_one_result = part_one(&data, 101, 103, 100);
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&data, 101, 103);
    println!("Part 2 Solution = {part_two_result}");
}
