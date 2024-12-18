use std::fs;

use day18::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day18.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must exist");

    let map = parse(&input, (70, 70).into(), 1024);
    let part_one_result = part_one(&map, (0, 0).into(), (70, 70).into());
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&input, (0, 0).into(), (70, 70).into());
    println!("Part 2 Solution = {part_two_result}");
}
