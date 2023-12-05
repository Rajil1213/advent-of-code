use std::fs;

use day05::{
    find_mapped_value, find_nearest_from_ranges, get_groups_of_two, get_seeds, parse_map_lines,
};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let first_line = contents
        .lines()
        .next()
        .expect("input should contain at least one line");

    let entire_input = contents.lines().collect::<Vec<&str>>();

    let seeds = get_seeds(first_line);
    let maps = parse_map_lines(entire_input);

    let mut min_location = usize::MAX;

    for seed in seeds {
        let location = find_mapped_value(&maps, seed);
        if location.lt(&min_location) {
            min_location = location
        }
    }

    println!("The nearest location is {min_location}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let first_line = contents
        .lines()
        .next()
        .expect("input should contain at least one line");

    let entire_input = contents.lines().collect::<Vec<&str>>();
    let seeds = get_groups_of_two(get_seeds(first_line));
    let maps = parse_map_lines(entire_input);

    let location = find_nearest_from_ranges(seeds, &maps);

    println!("The nearest location is {location}");
}
