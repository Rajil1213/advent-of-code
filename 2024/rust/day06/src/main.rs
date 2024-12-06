use std::fs;

use day06::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day06.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("must be able to read file");
    let mut map = parse(&input);

    let part_one_result = part_one(&mut map);
    println!("Part 1 Solution: {part_one_result}");

    let part_two_result = part_two(&mut map);
    println!("Part 2 Solution: {part_two_result}");
}
