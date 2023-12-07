use std::fs;

use day07::{parse_line, Card, Hand, NUM_CARDS_IN_HAND};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut hands: Vec<Hand> = vec![];
    for line in contents.lines() {
        hands.push(parse_line(line, false));
    }

    hands.sort();

    let mut total_winnings: usize = 0;
    for (rank, hand) in hands.iter().enumerate() {
        total_winnings += (rank + 1) * hand.bid;
    }

    println!("Total Winnings = {total_winnings}");
}

fn part_two(path: &str) {
    let contents = fs::read_to_string(path).expect("input file must be readable");

    let mut hands: Vec<Hand> = vec![];
    for line in contents.lines() {
        hands.push(parse_line(line, true));
    }

    // replace `J` with `Joker`
    hands = hands
        .into_iter()
        .map(|hand| {
            let contents = hand
                .contents
                .into_iter()
                .map(|card| if card == Card::J { Card::Joker } else { card })
                .collect::<Vec<Card>>();

            let contents: &[Card; NUM_CARDS_IN_HAND] = contents[..]
                .try_into()
                .expect("contents should have exactly {NUM_CARDS_IN_HAND} cards");

            Hand {
                contents: contents.clone(),
                kind: hand.kind,
                bid: hand.bid,
            }
        })
        .collect::<Vec<Hand>>();

    hands.sort();

    let mut total_winnings: usize = 0;
    for (rank, hand) in hands.iter().enumerate() {
        total_winnings += (rank + 1) * hand.bid;
    }

    println!("Total Winnings With Joker = {total_winnings}");
}
