use std::fs;

use day16::{count_energized, parse, traverse_grid, Direction};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut grid = parse(&contents);
    traverse_grid((0, 0), &Direction::Right, &mut grid);

    let count = count_energized(&grid);

    println!("Energized grids = {count}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let original_grid = parse(&contents);
    let width = original_grid.len();

    let mut maximum = 0;

    // check all on the top row:
    for i in 0..width {
        let mut current_grid = original_grid.clone();
        traverse_grid((0, i), &Direction::Down, &mut current_grid);

        let count = count_energized(&current_grid);
        if maximum < count {
            maximum = count;
        }
    }

    // check all on the bottom row:
    for i in 0..width {
        let mut current_grid = original_grid.clone();
        traverse_grid((width - 1, i), &Direction::Up, &mut current_grid);

        let count = count_energized(&current_grid);
        if maximum < count {
            maximum = count;
        }
    }

    // check all on the left column:
    for i in 0..width {
        let mut current_grid = original_grid.clone();
        traverse_grid((i, 0), &Direction::Right, &mut current_grid);

        let count = count_energized(&current_grid);
        if maximum < count {
            maximum = count;
        }
    }

    // check all on the right column:
    for i in 0..width {
        let mut current_grid = original_grid.clone();
        traverse_grid((i, width - 1), &Direction::Left, &mut current_grid);

        let count = count_energized(&current_grid);
        if maximum < count {
            maximum = count;
        }
    }

    println!("Maximum possible energized beams = {maximum}");
}
