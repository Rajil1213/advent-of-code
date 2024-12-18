use std::fmt::Display;
use std::ops::Not;
use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Elem {
    Safe,
    Unsafe,
}

impl From<char> for Elem {
    fn from(value: char) -> Self {
        match value {
            '#' => Self::Unsafe,
            '.' => Self::Safe,
            c => unreachable!("invalid char: {c}"),
        }
    }
}

impl Display for Elem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Elem::Safe => write!(f, "."),
            Elem::Unsafe => write!(f, "#"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
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
                x: bounds.x.min(self.x + 1),
                y: self.y,
            },
            Self {
                x: self.x,
                y: self.y.saturating_sub(1),
            },
            Self {
                x: self.x,
                y: bounds.y.min(self.y + 1),
            },
        ])
    }
}

pub type Map = Vec<Vec<Elem>>;

pub fn parse(input: &str, bounds: Position, num_fallen: usize) -> Map {
    let mut map: Map = vec![];

    let given_positions = input
        .lines()
        .take(num_fallen)
        .map(|line| {
            let mut values = line.split(",").take(2);

            let x = values.next().unwrap().parse().unwrap();
            let y = values.next().unwrap().parse().unwrap();

            Position { x, y }
        })
        .collect::<HashSet<Position>>();

    for from_top in 0..bounds.y + 1 {
        let mut row = vec![];
        for from_left in 0..bounds.x + 1 {
            let position = Position::from((from_left, from_top));
            if given_positions.contains(&position) {
                row.push(Elem::Unsafe);
            } else {
                row.push(Elem::Safe);
            }
        }

        map.push(row);
    }

    map
}

pub fn get_map(map: &Map) -> String {
    let mut grid = String::new();

    for ys in map {
        for x in ys {
            grid.push_str(&format!("{x}"));
        }

        grid.push('\n');
    }

    grid
}

pub fn part_one(map: &Map, starting_position: Position, final_position: Position) -> isize {
    // Apply Djikstra's Algorithm

    let mut visited: HashSet<Position> = HashSet::new();
    visited.insert(starting_position);

    let mut cur_cost = 0;

    let mut cost_q: BinaryHeap<Reverse<(isize, Position)>> = BinaryHeap::new();
    cost_q.push(Reverse((cur_cost, starting_position)));

    while !cost_q.is_empty() {
        let (cost, position) = cost_q.pop().unwrap().0;

        cur_cost = cost;

        if position == final_position {
            break;
        }

        let next_positions = position.next(&final_position);

        for neighbor in next_positions {
            if map[neighbor.y][neighbor.x] == Elem::Unsafe {
                continue;
            }

            if visited.contains(&neighbor) {
                continue;
            }

            visited.insert(neighbor);

            cost_q.push(Reverse((cur_cost + 1, neighbor)));
        }
    }

    if visited.contains(&final_position).not() {
        return -1;
    }

    cur_cost
}

pub fn part_two(input: &str, starting_position: Position, final_position: Position) -> String {
    let total_bytes = input.lines().count();

    for num_fallen in 0..total_bytes {
        let map = parse(input, final_position, num_fallen);
        let actual = part_one(&map, starting_position, final_position);

        if actual == -1 {
            return input.lines().nth(num_fallen - 1).unwrap().to_string();
        }
    }

    "0,0".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
    (r#"5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"#, 12),
        r#"...#...
..#..#.
....#..
...#..#
..#..#.
.#..#..
#.#....
"#
    )]
    fn test_parse(#[case] (input, num_fallen): (&str, usize), #[case] expected: &str) {
        let bounds = Position { x: 6, y: 6 };
        let map = parse(input, bounds, num_fallen);

        let actual = get_map(&map);

        assert_eq!(actual, expected.to_string());
    }

    #[rstest]
    #[case(
        (r#"5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"#, 12),
        22
    )]
    fn test_part_one(#[case] (input, num_fallen): (&str, usize), #[case] expected: isize) {
        let final_position = Position { x: 6, y: 6 };
        let map = parse(input, final_position, num_fallen);

        let actual = part_one(&map, Position { x: 0, y: 0 }, final_position);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"#,
        "6,1"
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: String) {
        let actual = part_two(input, (0, 0).into(), (6, 6).into());

        assert_eq!(actual, expected);
    }
}



