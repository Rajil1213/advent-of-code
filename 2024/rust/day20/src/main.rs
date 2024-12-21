use std::fs;

use day20::{parse, part_one};

const INPUT_FILE: &str = "input/day20.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must be present");
    let (map, starting_position, final_position) = parse(&input);

    let part_one_result = part_one(&map, starting_position, final_position, 100);
    println!("Part 1 Solution = {part_one_result}");
}
