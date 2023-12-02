use std::{collections::HashMap, str::FromStr, usize};

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Cube {
    Red,
    Blue,
    Green,
}

impl ToString for Cube {
    fn to_string(&self) -> String {
        match self {
            Cube::Red => "red".to_string(),
            Cube::Blue => "blue".to_string(),
            Cube::Green => "green".to_string(),
        }
    }
}

impl FromStr for Cube {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "red" => Ok(Cube::Red),
            "blue" => Ok(Cube::Blue),
            "green" => Ok(Cube::Green),
            _ => Err(format!("invalid input {s}")),
        }
    }
}

/// This function checks if the `game` is possible for the given `contents` in the bag.
///
/// # Example
/// ```
/// use day02::{check_if_possible, Cube};
/// use std::collections::HashMap;
///
/// let contents = HashMap::from([(Cube::Red, 4)]);
/// let possible_game = HashMap::from([(Cube::Red, 4)]);
/// assert_eq!(true, check_if_possible(contents.clone(), possible_game));
///
/// let impossible_game = HashMap::from([(Cube::Red, 5)]);
/// assert_eq!(false, check_if_possible(contents, impossible_game));
/// ```
pub fn check_if_possible(contents: &HashMap<Cube, usize>, game: &HashMap<Cube, usize>) -> bool {
    game.get(&Cube::Red).unwrap_or(&0) <= contents.get(&Cube::Red).unwrap_or(&0)
        && game.get(&Cube::Blue).unwrap_or(&0) <= contents.get(&Cube::Blue).unwrap_or(&0)
        && game.get(&Cube::Green).unwrap_or(&0) <= contents.get(&Cube::Green).unwrap_or(&0)
}

/// This function gets the count map of the cube for each `game` of the format:
/// [<int> "blue" | "green"" | "red"], [<int> "blue" | "green" |"red"]
///
/// # Example
/// ```
/// use day02::{Cube, get_cube_count};
/// use std::collections::HashMap;
///
/// let game = "1 red, 2 blue, 3 green";
/// let expected = HashMap::from([(Cube::Red, 1), (Cube::Blue, 2), (Cube::Green, 3)]);
/// assert_eq!(expected, get_cube_count(game));
/// ```
pub fn get_cube_count(game: &str) -> HashMap<Cube, usize> {
    let mut game_map = HashMap::new();

    game.split(", ").for_each(|entry| {
        let count_and_variant = entry.split(' ').collect::<Vec<&str>>();
        assert_eq!(
            count_and_variant.len(),
            2,
            "incorrect game format for {entry}"
        );

        let count = count_and_variant[0]
            .parse::<usize>()
            .expect("must be a valid count");
        let variant = Cube::from_str(count_and_variant[1]).expect("must be a valid color");

        game_map.insert(variant, count);
    });

    game_map
}

/// This function returns the game number and the game (cube count) from the given line
///
/// Example
/// ```
/// use day02::{Cube, get_game};
/// use std::collections::HashMap;
///
/// let entry = "Game 1: 3 blue, 5 green; 2 red";
/// let actual = get_game(entry);
/// assert_eq!(actual.0, 1);
/// assert_eq!(actual.1, "3 blue, 5 green; 2 red");
/// ```
pub fn get_game(entry: &str) -> (u64, &str) {
    let num_and_game = entry.split(": ").collect::<Vec<&str>>();
    assert_eq!(num_and_game.len(), 2, "incorrect game entry {entry}");

    let game_num = num_and_game[0].split(' ').collect::<Vec<&str>>();
    assert_eq!(
        game_num.len(),
        2,
        "incorrect game number format: {game_num:?}"
    );

    let game_num = game_num[1].parse::<u64>().unwrap_or_else(|_| {
        panic!("invalid game num: {}", game_num[1]);
    });

    (game_num, num_and_game[1])
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(HashMap::from([(Cube::Red, 2)]), true)]
    #[case(HashMap::from([(Cube::Blue, 4)]), true)]
    #[case(HashMap::from([(Cube::Green, 6)]), false)]
    fn checks_possibility_correctly(#[case] game: HashMap<Cube, usize>, #[case] expected: bool) {
        let contents: HashMap<Cube, usize> =
            HashMap::from([(Cube::Red, 3), (Cube::Blue, 4), (Cube::Green, 5)]);
        assert_eq!(expected, check_if_possible(&contents, &game))
    }

    #[rstest]
    #[case("1 blue, 3 red, 5 green", HashMap::from([(Cube::Blue, 1), (Cube::Red, 3), (Cube::Green, 5)]))]
    #[case("3 red, 5 green", HashMap::from([(Cube::Red, 3), (Cube::Green, 5)]))]
    #[case("5 green", HashMap::from([(Cube::Green, 5)]))]
    fn creates_cube_count_from_entry(#[case] entry: &str, #[case] expected: HashMap<Cube, usize>) {
        assert_eq!(expected, get_cube_count(entry));
    }
}
