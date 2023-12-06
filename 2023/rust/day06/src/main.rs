use std::{fs, iter::zip};

use day06::ways_to_win;

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let lines = contents.lines().collect::<Vec<&str>>();
    assert!(lines.len().eq(&2), "file must have exactly two lines");

    let time_line = lines[0]
        .split(':')
        .last()
        .expect("first line must contain a colon")
        .split(' ')
        .filter_map(|v| {
            if v.trim().is_empty() {
                return None;
            }
            Some(v.parse::<usize>().expect("must be a number"))
        })
        .collect::<Vec<usize>>();

    let distance_line = lines[1]
        .split(':')
        .last()
        .expect("first line must contain a colon")
        .split(' ')
        .filter_map(|v| {
            if v.trim().is_empty() {
                return None;
            }
            Some(v.parse::<usize>().expect("must be a number"))
        })
        .collect::<Vec<usize>>();

    assert!(time_line.len().eq(&distance_line.len()));

    let mut product = 1;
    for (total_race_time, distance_to_beat) in zip(time_line, distance_line) {
        let num_ways_to_win = ways_to_win(distance_to_beat, total_race_time);
        product *= num_ways_to_win;
    }

    println!("Product of Ways to win = {product}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let lines = contents.lines().collect::<Vec<&str>>();
    assert!(lines.len().eq(&2), "file must have exactly two lines");

    let total_race_time = lines[0]
        .split(':')
        .last()
        .expect("first line must contain a colon")
        .split(' ')
        .filter(|v| !v.is_empty())
        .collect::<String>()
        .parse::<usize>()
        .expect("must be a valid number");

    let distance_to_beat = lines[1]
        .split(':')
        .last()
        .expect("first line must contain a colon")
        .split(' ')
        .filter(|v| !v.is_empty())
        .collect::<String>()
        .parse::<usize>()
        .expect("must be a valid number");

    let num_ways_to_win = ways_to_win(distance_to_beat, total_race_time);

    println!("Total Number of Ways to win = {num_ways_to_win}");
}
