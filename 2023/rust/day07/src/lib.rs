use core::panic;
use std::{cmp::Ordering, collections::HashSet};

pub const NUM_CARDS_IN_HAND: usize = 5;

// PART ONE

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Hash, Clone)]
pub enum Card {
    A,
    K,
    Q,
    J,
    T,
    Nine,
    Eight,
    Seven,
    Six,
    Five,
    Four,
    Three,
    Two,
    Joker, // used to replace `J` in part two
}

impl Card {
    pub fn from(val: &char) -> Card {
        match val {
            'A' => Card::A,
            'K' => Card::K,
            'Q' => Card::Q,
            'J' => Card::J,
            'T' => Card::T,
            '9' => Card::Nine,
            '8' => Card::Eight,
            '7' => Card::Seven,
            '6' => Card::Six,
            '5' => Card::Five,
            '4' => Card::Four,
            '3' => Card::Three,
            '2' => Card::Two,
            _ => panic!("invalid card {val}"),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Kind {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

impl Kind {
    pub fn from_hand(hand: &[Card; NUM_CARDS_IN_HAND]) -> Kind {
        let mut unique_cards: HashSet<Card> = HashSet::new();
        for card in hand {
            unique_cards.insert(card.to_owned());
        }

        match unique_cards.len() {
            1 => Kind::FiveOfAKind,
            2 => {
                let count_of_either = hand
                    .iter()
                    .filter(|c| {
                        *c == unique_cards
                            .iter()
                            .next()
                            .expect("must have at least one element")
                    })
                    .collect::<Vec<&Card>>()
                    .len();

                match count_of_either {
                    4 | 1 => Kind::FourOfAKind,
                    3 | 2 => Kind::FullHouse,
                    _ => panic!("impossible hand!"),
                }
            }
            3 => {
                let mut has_three = false;
                for card in unique_cards {
                    if hand
                        .iter()
                        .filter(|c| *c == &card)
                        .collect::<Vec<&Card>>()
                        .len()
                        == 3
                    {
                        has_three = true;
                        break;
                    }
                }

                match has_three {
                    true => Kind::ThreeOfAKind,
                    false => Kind::TwoPair,
                }
            }

            4 => Kind::OnePair,
            5 => Kind::HighCard,
            _ => panic!("invalid hand"),
        }
    }

    pub fn from_hand_with_joker(hand: &[Card; NUM_CARDS_IN_HAND]) -> Kind {
        let mut unique_cards: HashSet<Card> = HashSet::new();
        let mut joker_count: usize = 0;
        for card in hand {
            unique_cards.insert(card.to_owned());
            if card == &Card::J {
                joker_count += 1;
            }
        }

        match unique_cards.len() {
            1 => Kind::FiveOfAKind,
            2 => {
                let count_of_either = hand
                    .iter()
                    .filter(|c| {
                        *c == unique_cards
                            .iter()
                            .next()
                            .expect("must have at least one element")
                    })
                    .collect::<Vec<&Card>>()
                    .len();

                match count_of_either {
                    4 | 1 => match joker_count {
                        1 => Kind::FiveOfAKind, // 1 imitates the remaining four
                        4 => Kind::FiveOfAKind, // 4 jokers imitate the remaining one
                        0 => Kind::FourOfAKind,
                        _ => panic!("impossible hand joker count neither 4 nor 1"),
                    },
                    3 | 2 => match joker_count {
                        2 => Kind::FiveOfAKind, // 2 jokers imitate the three
                        3 => Kind::FiveOfAKind, // all 3 jokers imitate the other two
                        _ => Kind::FullHouse,
                    },
                    _ => panic!("impossible hand; count of either none of 1, 2, 3, 4!"),
                }
            }
            3 => {
                let mut three_of_a_kind = false;
                for card in unique_cards {
                    if hand
                        .iter()
                        .filter(|c| *c == &card)
                        .collect::<Vec<&Card>>()
                        .len()
                        == 3
                    {
                        three_of_a_kind = true;
                        break;
                    }
                }

                match three_of_a_kind {
                    true => match joker_count {
                        1 => Kind::FourOfAKind,  // 1 joker imitates the three
                        3 => Kind::FourOfAKind,  // 3 jokers imitate one of the two
                        _ => Kind::ThreeOfAKind, // 3 or 0 jokers can't do anything
                    },
                    false => match joker_count {
                        1 => Kind::FullHouse,   // 1 joker matches one of the pairs
                        2 => Kind::FourOfAKind, // 2 jokers match the other pair
                        _ => Kind::TwoPair,
                    },
                }
            }

            4 => match joker_count {
                1 => Kind::ThreeOfAKind, // the joker imitates the pair
                2 => Kind::ThreeOfAKind, // each joker imitates one of the cards to form a triplet
                _ => Kind::OnePair,      // the joker pair can't do anything
            },
            5 => match joker_count {
                1 => Kind::OnePair, // one joker imitates one other card
                _ => Kind::HighCard,
            },
            _ => panic!("invalid hand"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Hand {
    pub contents: [Card; NUM_CARDS_IN_HAND],
    pub kind: Kind,
    pub bid: usize,
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.kind.cmp(&other.kind) {
            Ordering::Less => Ordering::Greater,
            Ordering::Greater => Ordering::Less,
            Ordering::Equal => {
                for i in 0..NUM_CARDS_IN_HAND {
                    match self.contents[i].cmp(&other.contents[i]) {
                        Ordering::Equal => continue,
                        Ordering::Less => return Ordering::Greater,
                        Ordering::Greater => return Ordering::Less,
                    }
                }

                Ordering::Equal
            }
        }
    }
}

pub fn parse_line(line: &str, joker: bool) -> Hand {
    let hand_and_bid = line.split(' ').collect::<Vec<&str>>();
    assert!(
        hand_and_bid.len().eq(&2),
        "each line should have a hand and bid with a space between"
    );

    let hand = hand_and_bid[0];
    let bid = hand_and_bid[1]
        .parse::<usize>()
        .expect("bid must be a number");

    assert!(
        hand.len().eq(&NUM_CARDS_IN_HAND),
        "each hand must have {NUM_CARDS_IN_HAND} cards"
    );
    let mut hand_chars = hand.chars();

    let contents: [Card; 5] = [
        Card::from(&hand_chars.next().expect("hand must have first card")),
        Card::from(&hand_chars.next().expect("hand must have second card")),
        Card::from(&hand_chars.next().expect("hand must have third card")),
        Card::from(&hand_chars.next().expect("hand must have fourth card")),
        Card::from(&hand_chars.next().expect("hand must have fifth card")),
    ];

    let kind = if joker {
        Kind::from_hand_with_joker(&contents)
    } else {
        Kind::from_hand(&contents)
    };

    Hand {
        contents,
        kind,
        bid,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case('A', Card::A)]
    #[case('T', Card::T)]
    #[case('2', Card::Two)]
    #[case('5', Card::Five)]
    fn gets_card_value_correctly(#[case] card: char, #[case] value: Card) {
        assert_eq!(Card::from(&card), value);
    }

    #[rstest]
    #[case(&[Card::Three, Card::Two, Card::T, Card::Three, Card::K], Kind::OnePair)]
    #[case(&[Card::T, Card::Five, Card::Five, Card::J, Card::Five], Kind::ThreeOfAKind)]
    #[case(&[Card::K, Card::K, Card::Six, Card::Seven, Card::Seven], Kind::TwoPair)]
    #[case(&[Card::K, Card::T, Card::J, Card::J, Card::T], Kind::TwoPair)]
    #[case(&[Card::Q, Card::Q, Card::Q, Card::J, Card::A], Kind::ThreeOfAKind)]
    fn calculates_kind_correctly(#[case] hand: &[Card; 5], #[case] kind: Kind) {
        assert_eq!(Kind::from_hand(hand), kind);
    }

    #[rstest]
    #[case((Hand { contents: [Card::Three, Card::Two, Card::T, Card::Three, Card::K], kind: Kind::OnePair, bid: 0 }, Hand { contents: [Card::T, Card::Five, Card::Five, Card::J, Card::Five], kind: Kind::ThreeOfAKind, bid: 0 }), Ordering::Less)]
    #[case((Hand { contents: [Card::K, Card::K, Card::Six, Card::Seven, Card::Seven], kind: Kind::TwoPair, bid: 0 }, Hand { contents: [Card::K, Card::T, Card::J, Card::J, Card::T], kind: Kind::TwoPair, bid: 0 }), Ordering::Greater)]
    #[case((Hand { contents: [Card::K, Card::K, Card::Six, Card::Seven, Card::Seven], kind: Kind::TwoPair, bid: 0 }, Hand { contents: [Card::K, Card::K, Card::Six, Card::Seven, Card::Seven], kind: Kind::TwoPair, bid: 0 }), Ordering::Equal)]
    fn compares_hands_correctly(#[case] hands: (Hand, Hand), #[case] expected: Ordering) {
        assert_eq!(hands.0.cmp(&hands.1), expected)
    }

    #[rstest]
    #[case(&[Card::Three, Card::Two, Card::T, Card::Three, Card::K], Kind::OnePair)]
    #[case(&[Card::T, Card::Five, Card::Five, Card::J, Card::Five], Kind::FourOfAKind)]
    #[case(&[Card::K, Card::K, Card::Six, Card::Seven, Card::Seven], Kind::TwoPair)]
    #[case(&[Card::K, Card::T, Card::J, Card::J, Card::T], Kind::FourOfAKind)]
    #[case(&[Card::Q, Card::Q, Card::Q, Card::J, Card::A], Kind::FourOfAKind)]
    fn calculates_kind_with_joker_correctly(#[case] hand: &[Card; 5], #[case] kind: Kind) {
        assert_eq!(Kind::from_hand_with_joker(hand), kind);
    }
}
