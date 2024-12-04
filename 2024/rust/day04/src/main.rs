use std::fs;

use day04::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day04.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).unwrap();
    let grid = parse(&input);

    let text_to_find: &str = "XMAS";
    let part_one_result = part_one(&grid, text_to_find);
    println!("Part 1 Solution: {part_one_result}");

    let text_to_find: &str = "MAS";
    let part_two_result = part_two(&grid, text_to_find);
    println!("Part 2 Solution: {part_two_result}");
}
