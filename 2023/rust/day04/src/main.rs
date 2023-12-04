use std::fs;

use day04::{calculate_points, calculate_total_cards, parse};

const INPUT_DIR: &str = "input";

fn main() {
    part_01(&format!("{INPUT_DIR}/part_one_input.txt"));
    part_two(&format!("{INPUT_DIR}/part_two_input.txt"));
}

fn part_01(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut sum = 0;
    for line in contents.lines() {
        let (winning, inhand) = parse(line);
        let points = calculate_points(winning, inhand);

        sum += points;
    }

    println!("Total points = {sum}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let total_cards = calculate_total_cards(&contents);

    println!("Total cards = {total_cards}");
}
