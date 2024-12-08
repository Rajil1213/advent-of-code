use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Antenna {
    Lower(char),
    Upper(char),
    Number(char),
    Dot,
}

impl Antenna {
    pub fn from(val: char) -> Antenna {
        match val {
            ('0'..='9') => Antenna::Number(val),
            ('a'..='z') => Antenna::Lower(val),
            ('A'..='Z') => Antenna::Upper(val),
            '.' => Antenna::Dot,
            _ => unreachable!("malformed input"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    x: isize,
    y: isize,
}

impl From<(usize, usize)> for Position {
    fn from(value: (usize, usize)) -> Self {
        Self {
            x: value.0 as isize,
            y: value.1 as isize,
        }
    }
}

impl From<(isize, isize)> for Position {
    fn from(value: (isize, isize)) -> Self {
        Self {
            x: value.0,
            y: value.1,
        }
    }
}

impl From<Position> for (usize, usize) {
    fn from(value: Position) -> Self {
        (value.x as usize, value.y as usize)
    }
}

impl Position {
    pub fn antinode(&self, other: &Position) -> (Self, Self) {
        let delta_x = self.x - other.x;
        let delta_y = self.y - other.y;

        let (next_x1, next_x2) = (self.x + delta_x, other.x - delta_x);
        let (next_y1, next_y2) = (self.y + delta_y, other.y - delta_y);

        ((next_x1, next_y1).into(), (next_x2, next_y2).into())
    }

    pub fn is_valid(&self, max_position: &Position) -> bool {
        (self.x >= 0 && self.x < max_position.x) && (self.y >= 0 && self.y < max_position.y)
    }

    pub fn antinodes_with_harmonics(
        &self,
        other: &Position,
        max_position: &Position,
    ) -> Vec<Position> {
        let delta_x = self.x - other.x;
        let delta_y = self.y - other.y;

        let (next_x1s, next_x2s) = (
            Self::get_possible_locs(self.x, delta_x, max_position.x),
            Self::get_possible_locs(other.x, -delta_x, max_position.x),
        );

        let (next_y1s, next_y2s) = (
            Self::get_possible_locs(self.y, delta_y, max_position.y),
            Self::get_possible_locs(other.y, -delta_y, max_position.y),
        );

        let antinodes_1 = next_x1s.into_iter().zip(next_y1s).map(Position::from);

        let mut antinodes_2 = next_x2s
            .into_iter()
            .zip(next_y2s)
            .map(Position::from)
            .collect::<Vec<Position>>();

        antinodes_2.extend(antinodes_1);

        antinodes_2
    }

    fn get_possible_locs(value: isize, delta: isize, max_val: isize) -> Vec<isize> {
        let mut next_val = value;
        let mut next_vals = vec![next_val];

        loop {
            next_val += delta;

            if next_val >= max_val || next_val < 0 {
                break;
            }

            next_vals.push(next_val);
        }

        next_vals
    }
}

pub fn parse(input: &str) -> (HashMap<Antenna, Vec<Position>>, Position) {
    (
        input
            .lines()
            .enumerate()
            .fold(HashMap::new(), |mut map, (row, line)| {
                map = line
                    .chars()
                    .enumerate()
                    .fold(map, |mut line_map, (col, c)| {
                        let antenna = Antenna::from(c);

                        if let Antenna::Dot = antenna {
                        } else {
                            line_map
                                .entry(antenna)
                                .and_modify(|v| v.push((row, col).into()))
                                .or_insert(vec![(row, col).into()]);
                        }

                        line_map
                    });

                map
            }),
        (input.lines().count(), input.lines().next().unwrap().len()).into(),
    )
}

pub fn part_one(
    input: &[String],
    map: &HashMap<Antenna, Vec<Position>>,
    max_position: &Position,
) -> usize {
    let mut antinodes = HashSet::new();

    for positions in map.values() {
        for index in 0..positions.len() {
            let starting_position = positions[index];
            for position in positions.iter().skip(index + 1) {
                let (antinode1, antinode2) = starting_position.antinode(position);

                if antinode1.is_valid(max_position) {
                    antinodes.insert(antinode1);
                }

                if antinode2.is_valid(max_position) {
                    antinodes.insert(antinode2);
                }
            }
        }
    }

    let mut antinoded_map = String::new();
    for (row, line) in input.iter().enumerate() {
        for (col, c) in line.chars().enumerate() {
            if antinodes.contains(&(row, col).into()) {
                antinoded_map.push('#');
            } else {
                antinoded_map.push(c);
            }
        }

        antinoded_map.push('\n');
    }

    println!("{}", antinoded_map);

    antinodes.len()
}

pub fn part_two(
    input: &[String],
    map: &HashMap<Antenna, Vec<Position>>,
    max_position: &Position,
) -> usize {
    let mut unique_antinodes = HashSet::new();

    for positions in map.values() {
        for index in 0..positions.len() {
            let starting_position = positions[index];
            for position in positions.iter().skip(index + 1) {
                let antinodes = starting_position.antinodes_with_harmonics(position, max_position);

                for antinode in antinodes {
                    unique_antinodes.insert(antinode);
                }
            }
        }
    }

    let mut antinoded_map = String::new();
    for (row, line) in input.iter().enumerate() {
        for (col, c) in line.chars().enumerate() {
            if unique_antinodes.contains(&(row, col).into()) {
                antinoded_map.push('#');
            } else {
                antinoded_map.push(c);
            }
        }

        antinoded_map.push('\n');
    }

    println!("{}", antinoded_map);

    unique_antinodes.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = r#"............
........0...
.....0......
.......0....
....0.......
......A.....
............
........A...
.........A..
...b........"#;

        let map = parse(input);

        let expected = HashMap::from([
            (
                Antenna::Number('0'),
                vec![(1usize, 8usize), (2, 5), (3, 7), (4, 4)]
                    .into_iter()
                    .map(|v| v.into())
                    .collect::<Vec<Position>>(),
            ),
            (
                Antenna::Upper('A'),
                vec![(5usize, 6usize), (7, 8), (8, 9)]
                    .into_iter()
                    .map(|v| v.into())
                    .collect::<Vec<Position>>(),
            ),
            (
                Antenna::Lower('b'),
                vec![(9usize, 3usize)]
                    .into_iter()
                    .map(|v| v.into())
                    .collect::<Vec<Position>>(),
            ),
        ]);

        assert_eq!(map.0, expected);
        assert_eq!(map.1, (10usize, 12usize).into());
    }

    #[test]
    fn test_part_one_and_two() {
        let input = r#"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"#;

        let (map, max_position) = parse(input);

        let input = input
            .lines()
            .map(|v| v.to_string())
            .collect::<Vec<String>>();

        let actual = part_one(&input, &map, &max_position);

        assert_eq!(actual, 14);

        let actual = part_two(&input, &map, &max_position);

        assert_eq!(actual, 34);
    }

    #[test]
    fn test_part_two() {
        let input = r#"T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
.........."#;

        let (map, max_position) = parse(input);

        let input = input
            .lines()
            .map(|v| v.to_string())
            .collect::<Vec<String>>();

        let actual = part_two(&input, &map, &max_position);

        assert_eq!(actual, 9);
    }
}
