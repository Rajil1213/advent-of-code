use std::fs;

use day11::{expand_space, find_distance_between_pairs, find_galaxies, load_image};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("Input file must be readable");

    let image = load_image(&contents);
    let expanded_image = expand_space(&image[..]);
    let galaxies = find_galaxies(&expanded_image);
    let distance_pairs = find_distance_between_pairs(galaxies);

    println!(
        "Sum of distance between galaxies: {}",
        distance_pairs.iter().sum::<usize>()
    );
}
