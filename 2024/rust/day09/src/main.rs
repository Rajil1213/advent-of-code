use std::fs;

use day09::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day09.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must exist");
    let (files, spaces, file_blocks, space_blocks) = parse(&input);

    let part_one_result = part_one(&files, &spaces);
    println!("Part 1 Solution: {part_one_result}");

    let part_two_result = part_two(&file_blocks, &space_blocks);
    println!("Part 2 Solution: {part_two_result}");
}
