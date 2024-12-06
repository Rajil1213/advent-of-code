use std::{collections::HashSet, ops::Not};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    x: isize,
    y: isize,
    direction: Direction,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub const OBSTACLE: char = '#';
pub const START: char = '^';
pub const VISITED: &str = "X";

pub type Map = Vec<String>;

impl Position {
    pub fn next(&mut self, map: &mut Map, starting_position: (isize, isize)) -> bool {
        let bound_x = map[0].len() as isize;
        let bound_y = map.len() as isize;

        let mut direction = self.direction;

        // check all possible paths (4 directions)
        for _ in 0..4 {
            let (delta_x, delta_y): (isize, isize) = match direction {
                Direction::Up => (-1, 0),
                Direction::Down => (1, 0),
                Direction::Left => (0, -1),
                Direction::Right => (0, 1),
            };

            let (next_x, next_y) = (self.x + delta_x, self.y + delta_y);

            if next_x < 0 || next_x >= bound_x || next_y < 0 || next_y >= bound_y {
                return false;
            }

            let (next_x, next_y) = (next_x as usize, next_y as usize);
            let next_loc = map[next_x].chars().nth(next_y).unwrap();

            if next_loc != OBSTACLE {
                *self = Self {
                    x: next_x as isize,
                    y: next_y as isize,
                    direction,
                };

                if self.x != starting_position.0 || self.y != starting_position.1 {
                    map[next_x].replace_range(next_y..next_y + 1, VISITED);
                }

                return true;
            }

            // change direction after encountering an obstacle
            direction = match direction {
                Direction::Up => Direction::Right,
                Direction::Down => Direction::Left,
                Direction::Left => Direction::Up,
                Direction::Right => Direction::Down,
            }
        }

        false // no viable path exists
    }
}

pub fn parse(input: &str) -> Map {
    input.lines().map(|line| line.to_string()).collect()
}

pub fn part_one(map: &mut Map) -> usize {
    let mut position = find_starting_pos(map);
    let starting_position = (position.x, position.y);

    while position.next(map, starting_position) {}

    let mut count = 0;
    for row in map {
        for col in row.chars() {
            if col == VISITED.chars().next().unwrap() {
                count += 1;
            }
        }
    }

    count + 1 // account for starting position
}

pub fn part_two(visited_map: &mut Map) -> usize {
    let mut count = 0;

    for (row, row_val) in visited_map.iter().enumerate() {
        for (col, col_val) in row_val.chars().enumerate() {
            if col_val == VISITED.chars().next().unwrap() {
                let mut new_map = visited_map.clone();
                new_map[row].replace_range(col..col + 1, OBSTACLE.to_string().as_str());

                if has_loop(&mut new_map) {
                    count += 1;
                }
            }
        }
    }

    count
}

pub fn has_loop(map: &mut Map) -> bool {
    let mut position = find_starting_pos(map);

    let mut visited = HashSet::new();
    visited.insert(position);

    while position.next(map, (position.x, position.y)) {
        if visited.insert(position).not() {
            return true;
        }
    }

    false
}

pub fn find_starting_pos(map: &Map) -> Position {
    for (row, row_val) in map.iter().enumerate() {
        for (col, col_val) in row_val.chars().enumerate() {
            if col_val == START {
                return Position {
                    x: row as isize,
                    y: col as isize,
                    direction: Direction::Up,
                };
            }
        }
    }

    unreachable!("could not find starting position");
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        r#"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."#,
        41
    )]
    fn part_one_works(#[case] input: &str, #[case] expected: usize) {
        let mut map = parse(input);

        let actual = part_one(&mut map);
        dbg!(map);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."#,
        6
    )]
    fn part_two_works(#[case] input: &str, #[case] expected: usize) {
        let mut map = parse(input);

        part_one(&mut map);
        dbg!(&map);

        let actual = part_two(&mut map);

        assert_eq!(actual, expected);
    }
}
