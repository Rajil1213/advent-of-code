use std::fs;

use day18::{calculate_lava_volume, create_actual_input, parse};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let input = fs::read_to_string(path).expect("input file must be readable");
    let dig_plan = parse(&input);

    // let matrix = create_matrix(&dig_plan);
    // convert_matrix_to_svg(&matrix, "real_path.svg");

    let lava_volume = calculate_lava_volume(&dig_plan);

    println!("Total lava volume = {lava_volume}");
}

fn part_two(path: &str) {
    let input = fs::read_to_string(path).expect("input file must be readable");
    let corrupted_dig_plan = parse(&input);
    let corrected_input = create_actual_input(&corrupted_dig_plan);
    let actual_dig_plan = parse(&corrected_input);

    let lava_volume = calculate_lava_volume(&actual_dig_plan);

    println!("Actual total lava volume = {lava_volume}");
}
