use std::{collections::HashMap, fs};

use day02::{check_if_possible, get_cube_count, get_game, Cube};

const INPUT_DIR: &str = "input";

fn main() {
    part_01(&format!("{INPUT_DIR}/part_one_values.txt"));
}

fn part_01(input_path: &str) {
    let contents = fs::read_to_string(input_path).expect("file must be readable");

    let bag_contents = HashMap::from([(Cube::Red, 12), (Cube::Blue, 14), (Cube::Green, 13)]);

    let mut sum = 0;
    for line in contents.lines() {
        let (game_num, games) = get_game(line);

        let mut all_possible = true;
        for game in games.split("; ") {
            let cube_count_map = get_cube_count(game);

            if !check_if_possible(&bag_contents, &cube_count_map) {
                all_possible = false;
                break;
            }
        }

        if all_possible {
            sum += game_num;
        }
    }

    println!("Sum of possible games = {sum}");
}
