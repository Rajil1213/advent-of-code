use std::fs;

use day11::{expanded_locations, find_distance_between_pairs, find_galaxies, load_image};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("Input file must be readable");
    const EXPANSION_FACTOR: usize = 1;

    let image = load_image(&contents);
    let galaxies = find_galaxies(&image);
    let expanded_locations = expanded_locations(&image[..]);
    let distance_pairs =
        find_distance_between_pairs(galaxies, expanded_locations, EXPANSION_FACTOR);

    println!(
        "Sum of distance between galaxies: {}",
        distance_pairs.iter().sum::<usize>()
    );
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("Input file must be readable");
    const EXPANSION_FACTOR: usize = 1000000;

    let image = load_image(&contents);
    let galaxies = find_galaxies(&image);
    let expanded_locations = expanded_locations(&image);
    let distance_pairs =
        find_distance_between_pairs(galaxies, expanded_locations, EXPANSION_FACTOR);

    println!(
        "Sum of distance between galaxies: {}",
        distance_pairs.iter().sum::<usize>()
    );
}
