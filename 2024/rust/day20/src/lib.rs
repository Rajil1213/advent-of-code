use std::fmt::Display;
use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Elem {
    Wall,
    Tile,
}

impl From<char> for Elem {
    fn from(value: char) -> Self {
        match value {
            '#' => Self::Wall,
            '.' | 'S' | 'E' => Self::Tile,
            c => unreachable!("invalid char: {c}"),
        }
    }
}

impl Display for Elem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Elem::Wall => write!(f, "#"),
            Elem::Tile => write!(f, "."),
        }
    }
}

impl Elem {
    pub fn is_wall(&self) -> bool {
        matches!(self, Self::Wall)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    x: usize,
    y: usize,
}

impl From<(usize, usize)> for Position {
    fn from(value: (usize, usize)) -> Self {
        Self {
            x: value.0,
            y: value.1,
        }
    }
}

impl From<Position> for (usize, usize) {
    fn from(value: Position) -> Self {
        (value.x, value.y)
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Position {
    // the boundary has a bunch of `#`'s, so no need to check boundary limits
    pub fn next(&self, bounds: &Position) -> HashSet<Position> {
        HashSet::from([
            Self {
                x: self.x.saturating_sub(1),
                y: self.y,
            },
            Self {
                x: (bounds.x - 1).min(self.x + 1),
                y: self.y,
            },
            Self {
                x: self.x,
                y: self.y.saturating_sub(1),
            },
            Self {
                x: self.x,
                y: (bounds.y - 1).min(self.y + 1),
            },
        ])
    }
}

pub type Map = Vec<Vec<Elem>>;

pub fn parse(input: &str) -> (Map, Position, Position) {
    let mut map: Map = vec![];
    let mut starting_position: Position = (0, 0).into();
    let mut final_position: Position = (0, 0).into();

    for (i, line) in input.lines().enumerate() {
        let mut row: Vec<Elem> = vec![];
        for (j, c) in line.chars().enumerate() {
            if c == 'S' {
                starting_position = (i, j).into();
            } else if c == 'E' {
                final_position = (i, j).into();
            }

            row.push(Elem::from(c));
        }

        map.push(row);
    }

    (map, starting_position, final_position)
}

pub fn part_one(
    map: &Map,
    starting_position: Position,
    final_position: Position,
    limit: usize,
) -> usize {
    let bounds = Position::from((map.len(), map[0].len()));

    let straight_path = find_path(map, starting_position, final_position, bounds);
    let cost_without_cheat = straight_path.len();

    let mut count = 0;
    let mut adjusted: HashSet<Position> = HashSet::new();
    for position in straight_path {
        for neighbor in position.next(&bounds) {
            let element = map[neighbor.x][neighbor.y];
            if adjusted.contains(&neighbor) {
                continue;
            }

            if element.is_wall() {
                let mut new_map = map.clone();
                new_map[neighbor.x][neighbor.y] = Elem::Tile;
                adjusted.insert(neighbor);

                let cost_with_cheat = djikstra(&new_map, starting_position, final_position, bounds);
                let saved = cost_without_cheat.saturating_sub(cost_with_cheat);

                if saved >= limit {
                    count += 1;
                }
            }
        }
    }

    count
}

fn find_path(
    map: &Map,
    starting_position: Position,
    final_position: Position,
    bounds: Position,
) -> Vec<Position> {
    let mut position = starting_position;
    let mut visited: HashSet<Position> = HashSet::from([starting_position]);
    let mut path: Vec<Position> = vec![];

    while position != final_position {
        for neighbor in position.next(&bounds) {
            if visited.contains(&neighbor) || map[neighbor.x][neighbor.y].is_wall() {
                continue;
            }

            visited.insert(neighbor);
            path.push(neighbor);
            position = neighbor;
        }
    }

    path
}

fn djikstra(
    map: &Map,
    starting_position: Position,
    final_position: Position,
    bounds: Position,
) -> usize {
    let mut visited: HashSet<Position> = HashSet::new();
    visited.insert(starting_position);

    let mut cur_cost = 0;

    let mut cost_q: BinaryHeap<Reverse<(usize, Position)>> = BinaryHeap::new();
    cost_q.push(Reverse((cur_cost, starting_position)));

    while !cost_q.is_empty() {
        let (cost, position) = cost_q.pop().unwrap().0;

        cur_cost = cost;

        if position == final_position {
            return cur_cost;
        }

        for neighbor in position.next(&bounds) {
            if visited.contains(&neighbor) {
                continue;
            }

            let element = map[neighbor.x][neighbor.y];

            if element.is_wall() {
                continue;
            }

            cost_q.push(Reverse((cur_cost + 1, neighbor)));

            visited.insert(neighbor);
        }
    }

    if !visited.contains(&final_position) {
        return usize::MAX;
    }

    cur_cost
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        (r#"###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"#, 10),
        10
    )]
    fn part_one_works(#[case] (input, limit): (&str, usize), #[case] expected: usize) {
        let (map, starting_position, final_position) = parse(input);

        let actual = part_one(&map, starting_position, final_position, limit);

        assert_eq!(actual, expected);
    }
}
