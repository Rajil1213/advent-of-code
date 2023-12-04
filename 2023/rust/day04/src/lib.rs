use std::{collections::BTreeMap, u32};

pub fn calculate_points(winning: Vec<u32>, inhand: Vec<u32>) -> u32 {
    let base: u32 = 2;
    let mut num_winning_cards = 0;

    for value in inhand {
        if winning.contains(&value) {
            num_winning_cards += 1;
        }
    }

    if num_winning_cards > 0 {
        return base.pow(num_winning_cards - 1);
    }

    0
}

/// Parse input to winning cards and cards in hand, presuming input to be of the format:
/// Card <num>: <num1> <num2> ... <numN> | <num1> <num2> ... <numN>
pub fn parse(input: &str) -> (Vec<u32>, Vec<u32>) {
    let all_cards = input
        .split(": ")
        .last()
        .expect("must have cards separated with : ");
    let all_cards = all_cards.split(" | ").collect::<Vec<&str>>();
    assert_eq!(all_cards.len(), 2, "must have card numbers separated by | ");

    let winning_cards = all_cards[0]
        .split(' ')
        .filter_map(|val| {
            if val.is_empty() {
                None
            } else {
                Some(val.parse::<u32>().expect("winning cards must be a number"))
            }
        })
        .collect::<Vec<u32>>();

    let cards_in_hand = all_cards[1]
        .split(' ')
        .filter_map(|val| {
            if val.is_empty() {
                None
            } else {
                Some(val.parse::<u32>().expect("cards in hand must be a number"))
            }
        })
        .collect::<Vec<u32>>();

    (winning_cards, cards_in_hand)
}

pub fn calculate_total_cards(input: &str) -> usize {
    let mut card_to_copies: BTreeMap<usize, usize> = BTreeMap::new();

    // each line number corresponds to the card number, so enumerating should be fine
    for (card_num, line) in input.lines().enumerate() {
        let (winning, inhand) = parse(line);

        let mut winning_cards_in_hand = 0;
        for inhand_card in inhand {
            if winning.contains(&inhand_card) {
                winning_cards_in_hand += 1
            }
        }

        // count the original
        let current_copies = card_to_copies
            .entry(card_num)
            .and_modify(|e| *e += 1)
            .or_insert(1)
            .to_owned();

        // insert or increment the copies for card number
        // on all subsequent cards for each point won
        // each copy of the current card contributes to the total
        for i in 1..winning_cards_in_hand + 1 {
            card_to_copies
                .entry(card_num + i)
                .and_modify(|e| *e += current_copies)
                .or_insert(current_copies);
        }
    }

    card_to_copies.values().sum::<usize>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", (vec![41, 48, 83, 86, 17], vec![83, 86,  6, 31, 17,  9, 48, 53]))]
    #[case("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", (vec![13, 32, 20, 16, 61], vec![61, 30, 68, 82, 17, 32, 24, 19]))]
    fn parses_input_correctly(#[case] line: &str, #[case] expected: (Vec<u32>, Vec<u32>)) {
        assert_eq!(parse(line), expected);
    }

    #[rstest]
    #[case((vec![1, 21, 53, 59, 44], vec![69, 82, 63, 72, 16, 21, 14, 1]), 2)]
    #[case((vec![31, 18, 13, 56, 72], vec![74, 77, 10, 23, 35, 67, 36, 11]), 0)]
    fn calculates_points_correctly(#[case] input: (Vec<u32>, Vec<u32>), #[case] expected: u32) {
        assert_eq!(calculate_points(input.0, input.1), expected);
    }

    #[test]
    fn calculates_total_cards_correctly() {
        let input: &str = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 87 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";

        assert_eq!(calculate_total_cards(input), 44);
    }
}
