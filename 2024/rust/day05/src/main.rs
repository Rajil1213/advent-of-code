use std::fs;

use day05::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day05.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).unwrap();
    let (order, entries) = parse(&input);

    let part_one_result = part_one(&order, &entries);
    println!("Part 1 Solution: {part_one_result}");

    let part_two_result = part_two(&order, &entries);
    println!("Part 2 Solution: {part_two_result}");
}
