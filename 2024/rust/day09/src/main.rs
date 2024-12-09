use std::fs;

use day09::{parse, part_one};

const INPUT_FILE: &str = "input/day09.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must exist");
    let (files, spaces) = parse(&input);

    let part_one_result = part_one(&files, &spaces);
    println!("Part 1 Solution: {part_one_result}");
}
