use std::u32;

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
}



