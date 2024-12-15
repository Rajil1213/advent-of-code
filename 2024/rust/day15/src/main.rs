use std::fs;

use day15::{parse, part_one, part_two};

const INPUT_FILE: &str = "input/day15.txt";

fn main() {
    let input = fs::read_to_string(INPUT_FILE).expect("input file must be present");
    let (map, initial_location, directions) = parse(&input);

    let part_one_result = part_one(&mut map.clone(), initial_location, directions.clone());
    println!("Part 1 Solution = {part_one_result}");

    let part_two_result = part_two(&map, directions);
    println!("Part 2 Solution = {part_two_result}");
}
