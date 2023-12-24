use std::fs;

use day18::{calculate_lava_volume, convert_matrix_to_svg, create_matrix, parse};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let input = fs::read_to_string(path).expect("input file must be readable");
    let dig_plan = parse(&input);

    let matrix = create_matrix(&dig_plan);
    convert_matrix_to_svg(&matrix, "real_path.svg");

    let lava_volume = calculate_lava_volume(&dig_plan);

    println!("Total lava volume = {lava_volume}");
}
