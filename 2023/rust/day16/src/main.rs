use std::fs;

use day16::{count_energized, parse, traverse_grid, Direction};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut grid = parse(&contents);
    traverse_grid((0, 0), &Direction::Right, &mut grid);

    let count = count_energized(&grid);

    println!("Energized grids = {count}");
}
