use std::{collections::HashMap, fs};

use day02::{check_if_possible, get_cube_count, get_game, Cube};

const INPUT_DIR: &str = "input";

fn main() {
    part_01(&format!("{INPUT_DIR}/part_one_values.txt"));
    part_02(&format!("{INPUT_DIR}/part_two_values.txt"));
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

fn part_02(input_path: &str) {
    let contents = fs::read_to_string(input_path).expect("file must be readable");

    let mut sum = 0;
    for line in contents.lines() {
        let (_game_num, games) = get_game(line);

        let mut min_cube_count = HashMap::from([(Cube::Red, 0), (Cube::Blue, 0), (Cube::Green, 0)]);
        for game in games.split("; ") {
            let cube_count_map = get_cube_count(game);

            if cube_count_map
                .get(&Cube::Red)
                .unwrap_or(&0)
                .gt(&min_cube_count[&Cube::Red])
            {
                min_cube_count.insert(Cube::Red, cube_count_map[&Cube::Red]);
            }

            if cube_count_map
                .get(&Cube::Blue)
                .unwrap_or(&0)
                .gt(&min_cube_count[&Cube::Blue])
            {
                min_cube_count.insert(Cube::Blue, cube_count_map[&Cube::Blue]);
            }

            if cube_count_map
                .get(&Cube::Green)
                .unwrap_or(&0)
                .gt(&min_cube_count[&Cube::Green])
            {
                min_cube_count.insert(Cube::Green, cube_count_map[&Cube::Green]);
            }
        }
        let power =
            min_cube_count[&Cube::Red] * min_cube_count[&Cube::Blue] * min_cube_count[&Cube::Green];
        sum += power;
    }

    println!("Sum of power of min possible games = {sum}");
}
