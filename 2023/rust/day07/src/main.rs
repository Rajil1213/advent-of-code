use std::fs;

use day07::{parse_line, Hand};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut hands: Vec<Hand> = vec![];
    for line in contents.lines() {
        hands.push(parse_line(line));
    }

    // dbg!(&hands);
    hands.sort();
    // dbg!(&hands);

    let mut total_winnings: usize = 0;
    for (rank, hand) in hands.iter().enumerate() {
        total_winnings += (rank + 1) * hand.bid;
    }

    println!("Total Winnings = {total_winnings}");
}
