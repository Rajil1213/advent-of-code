use std::fs;

use day10::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day10.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("must be able to read file");
    let (map, trail_heads, trail_ends) = parse(&input);

    let part_one_output = part_one(&map, &trail_heads, &trail_ends);
    println!("Part 1 Solution = {part_one_output}");

    let part_two_output = part_two(&map, &trail_heads, &trail_ends);
    println!("Part 2 Solution = {part_two_output}");
}
