use std::{collections::HashMap, str::FromStr};

#[derive(Debug, Clone, PartialEq)]
pub struct Value<'a> {
    left: &'a str,
    right: &'a str,
}

pub fn parse_line(line: &str) -> (&str, Value) {
    let key_values: Vec<&str> = line.split(" = ").collect::<Vec<&str>>();
    assert!(key_values.len().eq(&2), "key and values must be present");

    let key = key_values[0];
    let value: [&str; 2] = key_values[1]
        .trim_start_matches('(')
        .trim_end_matches(')')
        .split(", ")
        .collect::<Vec<&str>>()[..2]
        .try_into()
        .expect("tuple must be present");

    (
        key,
        Value {
            left: value[0],
            right: value[1],
        },
    )
}

pub fn parse_map(map_set: &str) -> HashMap<&str, Value> {
    let lines = map_set.lines();

    let mut map: HashMap<&str, Value> = HashMap::new();
    for line in lines {
        let (k, v) = parse_line(line);
        map.insert(k, v);
    }

    map
}

#[derive(Debug, Clone)]
pub enum Direction {
    Left,
    Right,
}

impl FromStr for Direction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "L" => Ok(Direction::Left),
            "R" => Ok(Direction::Right),
            _ => Err(format!("{s} is an invalid direction")),
        }
    }
}

pub struct InstructionSet(Vec<Direction>);

impl FromStr for InstructionSet {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            s.chars()
                .map(|c| Direction::from_str(&format!("{c}")).unwrap())
                .collect::<Vec<Direction>>(),
        ))
    }
}

pub fn find_steps(
    map: HashMap<&str, Value>,
    start: &str,
    stop: &str,
    InstructionSet(directions): InstructionSet,
) -> usize {
    let mut src = start;
    let mut dst: Option<&str> = None;
    let num_directions = directions.len();

    let mut step = 0;
    let mut num_steps = 0;
    while Some(stop) != dst {
        let next = &directions[step];
        let path = map
            .get(src)
            .unwrap_or_else(|| panic!("{src} must be present"))
            .clone();
        dst = match next {
            Direction::Left => Some(path.left),
            Direction::Right => Some(path.right),
        };
        src = dst.expect("new src must be valid");

        step = (step + 1) % num_directions;
        num_steps += 1;
    }

    num_steps
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("AAA = (BBB, CCC)", ("AAA", Value { left: "BBB", right: "CCC"}))]
    #[case("BBB = (DDD, EEE)", ("BBB", Value { left: "DDD", right: "EEE"}))]
    #[case("CCC = (ZZZ, GGG)", ("CCC", Value { left: "ZZZ", right: "GGG"}))]
    #[case("DDD = (DDD, DDD)", ("DDD", Value { left: "DDD", right: "DDD"}))]
    #[case("EEE = (EEE, EEE)", ("EEE", Value { left: "EEE", right: "EEE"}))]
    fn parses_line_correctly(#[case] line: &str, #[case] expected: (&str, Value)) {
        assert_eq!(parse_line(line), expected);
    }

    #[test]
    fn finds_steps_correctly() {
        let instruction_set = InstructionSet::from_str("LLR").expect("directions must be valid");

        let map_set: &str = "AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)";

        let map = parse_map(map_set);
        let expected_num_steps = 6;
        let num_steps = find_steps(map, "AAA", "ZZZ", instruction_set);

        assert_eq!(num_steps, expected_num_steps);

        let instruction_set = InstructionSet::from_str("RL").expect("directions must be valid");
        let map_set: &str = "AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)";

        let map = parse_map(map_set);
        let expected_num_steps = 2;
        let num_steps = find_steps(map, "AAA", "ZZZ", instruction_set);
        assert_eq!(num_steps, expected_num_steps);
    }
}
