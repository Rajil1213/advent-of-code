use std::fs;

use day09::{get_next_value, get_previous_value};

const INPUT_DIR: &str = "input";
fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut sum: i64 = 0;
    for line in contents.lines() {
        let numbers = line
            .split(' ')
            .map(|val| val.parse::<i64>().expect("value must be a number"))
            .collect::<Vec<i64>>();
        let next_num = get_next_value(&numbers);
        sum += next_num;
    }

    println!("Sum of next values = {sum}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut sum: i64 = 0;
    for line in contents.lines() {
        let numbers = line
            .split(' ')
            .map(|val| val.parse::<i64>().expect("value must be a number"))
            .collect::<Vec<i64>>();
        let previous_num = get_previous_value(&numbers);
        sum += previous_num;
    }

    println!("Sum of previous values = {sum}");
}
