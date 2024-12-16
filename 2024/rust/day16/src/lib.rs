use std::collections::HashMap;
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
            '.' => Self::Tile,
            'S' => Self::Tile,
            'E' => Self::Tile,
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
pub enum Direction {
    West,
    East,
    North,
    South,
}

impl Direction {
    pub fn next(&self) -> [Self; 3] {
        match self {
            Direction::West => [Self::West, Self::North, Self::South],
            Direction::East => [Self::East, Self::North, Self::South],
            Direction::North => [Self::North, Self::West, Self::East],
            Direction::South => [Self::South, Self::West, Self::East],
        }
    }

    pub fn cost(&self, next_direction: &Direction) -> isize {
        match (self, next_direction) {
            (a, b) if a == b => 1,
            (Self::West, Self::North) | (Self::West, Self::South) => 1001,
            (Self::East, Self::North) | (Self::East, Self::South) => 1001,
            (Self::North, Self::West) | (Self::North, Self::East) => 1001,
            (Self::South, Self::West) | (Self::South, Self::East) => 1001,
            _ => unreachable!("invalid direction: {self:?} -> {next_direction:?}"),
        }
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
    pub fn next(&self, direction: &Direction) -> Self {
        match direction {
            Direction::West => Self {
                x: self.x,
                y: self.y.saturating_sub(1),
            },
            Direction::East => Self {
                x: self.x,
                y: self.y + 1,
            },
            Direction::North => Self {
                x: self.x.saturating_sub(1),
                y: self.y,
            },
            Direction::South => Self {
                x: self.x + 1,
                y: self.y,
            },
        }
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
    direction: Direction,
) -> isize {
    // Apply Djikstra's Algorithm

    let mut visited: HashSet<Position> = HashSet::new();
    visited.insert(starting_position);

    let mut cur_cost = 0;
    let mut direction = direction;

    let mut cost_q: BinaryHeap<Reverse<(isize, Position, Direction)>> = BinaryHeap::new();
    cost_q.push(Reverse((cur_cost, starting_position, direction)));

    while !cost_q.is_empty() {
        let (cost, position, dir) = cost_q.pop().unwrap().0;

        cur_cost = cost;
        direction = dir;

        if position == final_position {
            break;
        }

        let neighbors_costs_directions = direction
            .next()
            .iter()
            .map(|dir| (position.next(dir), direction.cost(dir), *dir))
            .collect::<Vec<(Position, isize, Direction)>>();

        if neighbors_costs_directions.is_empty() {
            continue;
        }

        for (neighbor, cost, dir) in neighbors_costs_directions {
            if visited.contains(&neighbor) || map[neighbor.x][neighbor.y].is_wall() {
                continue;
            }

            visited.insert(neighbor);

            cost_q.push(Reverse((cur_cost + cost, neighbor, dir)));
        }
    }

    cur_cost
}

pub fn part_two(
    map: &Map,
    starting_position: Position,
    final_position: Position,
    direction: Direction,
) -> usize {
    let target_cost = part_one(map, starting_position, final_position, direction);

    let mut all_possible = HashMap::new();

    bfs(
        map,
        target_cost,
        starting_position,
        final_position,
        direction,
        &mut all_possible,
    );

    all_possible.insert((starting_position, 0), true);

    let just_true = all_possible
        .iter()
        .filter_map(|v| if *v.1 { Some(v.0 .0) } else { None })
        .collect::<HashSet<_>>();

    just_true.len()
}

fn bfs(
    map: &Map,
    target_cost: isize,
    starting_position: Position,
    final_position: Position,
    direction: Direction,
    memo: &mut HashMap<(Position, isize), bool>,
) -> bool {
    if let Some(reachable) = memo.get(&(starting_position, target_cost)) {
        return *reachable;
    }

    if target_cost < 0 {
        return false;
    }

    if starting_position == final_position {
        memo.insert((starting_position, target_cost), true);
        return true;
    }

    let neighbors_costs_directions = direction
        .next()
        .map(|dir| (starting_position.next(&dir), direction.cost(&dir), dir));

    let mut reachable = false;
    for (neighbor, cost, direction) in neighbors_costs_directions {
        if map[neighbor.x][neighbor.y].is_wall() {
            continue;
        }

        if bfs(
            map,
            target_cost - cost,
            neighbor,
            final_position,
            direction,
            memo,
        ) {
            reachable = true;
        }
    }

    memo.insert((starting_position, target_cost), reachable);

    reachable
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        r#"###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"#,
        7036
    )]
    #[case(
        r#"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"#,
        11048
    )]
    fn part_one_works(#[case] input: &str, #[case] expected: isize) {
        let (map, starting_position, final_position) = parse(input);

        let actual = part_one(&map, starting_position, final_position, Direction::East);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"#,
        45
    )]
    #[case(
        r#"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"#,
        64
    )]
    fn part_two_works(#[case] input: &str, #[case] expected: usize) {
        let (map, starting_position, final_position) = parse(input);

        let actual = part_two(&map, starting_position, final_position, Direction::East);

        assert_eq!(actual, expected);
    }
}
