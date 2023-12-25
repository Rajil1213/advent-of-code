use std::fs;

use day19::{parse, run, Status};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");
    let (workflow_map, parts) = parse(&contents);

    let mut sum = 0;
    for part in parts.iter() {
        if let Some(Status::Accepted) = run(part, &workflow_map) {
            sum += part.rating.x + part.rating.m + part.rating.a + part.rating.s;
        }
    }

    println!("Sum of ratings of accepted parts = {sum}");
}
