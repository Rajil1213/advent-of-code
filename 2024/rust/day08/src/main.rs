use std::fs;

use day08::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day08.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("must be able to read file");
    let (map, max_position) = parse(&input);

    let input = input
        .lines()
        .map(|line| line.to_string())
        .collect::<Vec<String>>();

    let part_one_result = part_one(&input, &map, &max_position);
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&input, &map, &max_position);
    println!("Part 2 Solution = {part_two_result}");
}
