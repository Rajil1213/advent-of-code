use std::fs;

use day16::{parse, part_one, part_two, Direction};

const INPUT_FILE: &str = "input/day16.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must exist");
    let (map, starting_position, final_position) = parse(&input);

    let part_one_result = part_one(&map, starting_position, final_position, Direction::East);
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&map, starting_position, final_position, Direction::East);
    println!("Part 2 Solution = {part_two_result}");
}
