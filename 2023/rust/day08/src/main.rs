use std::{fs, str::FromStr};

use day08::{find_steps, parse_map, InstructionSet};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut lines = contents.lines();
    let direction_line = lines
        .next()
        .expect("first line must be present in input file");
    let instruction_set =
        InstructionSet::from_str(direction_line).expect("directions must be valid");

    // consume blank line
    lines.next().expect("second line must be a blank line");

    let map_set = lines.collect::<Vec<&str>>().join("\n");

    let map = parse_map(&map_set);

    let num_steps = find_steps(map, "AAA", "ZZZ", instruction_set);

    println!("Num step = {num_steps}");
}
