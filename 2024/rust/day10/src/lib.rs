use std::fmt::Display;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Position {
    x: usize,
    y: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
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

impl Position {
    fn next_positions(&self, max_position: &Position) -> Vec<Position> {
        let x = self.x;
        let y = self.y;

        let max_x = max_position.x;
        let max_y = max_position.y;

        let mut next_positions = vec![];

        // left
        if y != 0 {
            next_positions.push(Position::from((x, y - 1)));
        }

        // right
        if (0..max_y).contains(&(y + 1)) {
            next_positions.push(Position::from((x, y + 1)));
        }

        // up
        if x != 0 {
            next_positions.push(Position::from((x - 1, y)));
        }

        // down
        if (0..max_x).contains(&(x + 1)) {
            next_positions.push(Position::from((x + 1, y)));
        }

        next_positions
    }

    fn is_valid(&self, next_position: &Position, map: &Map) -> bool {
        let cur_height = map[self.x][self.y];
        let next_height = map[next_position.x][next_position.y];

        next_height as i32 - cur_height as i32 == 1
    }
}

pub type Map = Vec<Vec<u32>>;
pub type TrailHeads = Vec<Position>;
pub type TrailEnds = Vec<Position>;

pub fn parse(input: &str) -> (Map, TrailHeads, TrailEnds) {
    input.lines().enumerate().fold(
        (Vec::new(), Vec::new(), Vec::new()),
        |(mut row_map, row_trail_heads, row_trail_ends), (row_num, row_val)| {
            let (col_map, col_trail_heads, col_trail_ends) = row_val.chars().enumerate().fold(
                (Vec::new(), row_trail_heads, row_trail_ends),
                |(mut col_map, mut col_trail_heads, mut col_trail_ends), (col_num, col_val)| {
                    let height = col_val.to_digit(10).unwrap_or(u32::MAX);

                    col_map.push(height);

                    if height == 0 {
                        col_trail_heads.push(Position::from((row_num, col_num)));
                    } else if height == 9 {
                        col_trail_ends.push(Position::from((row_num, col_num)));
                    }

                    (col_map, col_trail_heads, col_trail_ends)
                },
            );

            row_map.push(col_map);

            (row_map, col_trail_heads, col_trail_ends)
        },
    )
}

pub fn part_one(map: &Map, trail_heads: &TrailHeads, trail_ends: &TrailEnds) -> usize {
    let mut count = 0;
    for trail_head in trail_heads {
        for trail_end in trail_ends {
            if dfs(map, trail_head, trail_end) {
                count += 1;
            }
        }
    }

    count
}

pub fn part_two(map: &Map, trail_heads: &TrailHeads, trail_ends: &TrailEnds) -> usize {
    let mut rating = 0;
    for trail_head in trail_heads {
        for trail_end in trail_ends {
            rating += bfs(map, trail_head, trail_end, 0);
        }
    }

    rating
}

pub fn dfs(map: &Map, start_position: &Position, end_position: &Position) -> bool {
    if start_position == end_position {
        return true;
    }

    let max_position = Position::from((map.len(), map[0].len()));

    let next_positions = start_position.next_positions(&max_position);
    let next_valid_positions = next_positions
        .iter()
        .filter(|next_pos| start_position.is_valid(next_pos, map));

    for next_position in next_valid_positions {
        if dfs(map, next_position, end_position) {
            return true;
        }
    }

    false
}

pub fn bfs(map: &Map, start_position: &Position, end_position: &Position, count: usize) -> usize {
    if start_position == end_position {
        return 1;
    }

    let max_position = Position::from((map.len(), map[0].len()));

    let next_positions = start_position.next_positions(&max_position);
    let next_valid_positions = next_positions
        .iter()
        .filter(|next_pos| start_position.is_valid(next_pos, map))
        .collect::<Vec<_>>();

    let mut subtrees_count = 0;
    for next_position in next_valid_positions {
        let possible_paths = bfs(map, next_position, end_position, 0);

        subtrees_count += possible_paths;
    }

    count + subtrees_count
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(r#"110
076
987
809"#, (vec![
            Position::from((0, 2)),
            Position::from((1, 0)),
            Position::from((3, 1))
        ],
        vec![
            Position::from((2, 0)),
            Position::from((3, 2))
        ],
        vec![
            vec![1, 1, 0],
            vec![0, 7, 6],
            vec![9, 8, 7],
            vec![8, 0, 9]
        ]))]
    fn test_parse(#[case] input: &str, #[case] expected: (TrailHeads, TrailEnds, Map)) {
        let (actual_map, actual_trail_heads, actual_trail_ends) = parse(input);

        assert_eq!(actual_trail_heads, expected.0);
        assert_eq!(actual_trail_ends, expected.1);
        assert_eq!(actual_map, expected.2);
    }

    #[rstest]
    #[case(
        r#"0123
1234
8765
9896"#,
        2
    )]
    #[case(
        r#"...0...
...1...
...2...
6543456
7..4..7
8..5678
9.....9"#,
        2
    )]
    #[case(
        r#"..90..9
...1.98
...2..7
6543456
765.987
87678..
987...."#,
        4
    )]
    #[case(
        r#"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456932"#,
        35
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let (map, trail_heads, trail_ends) = parse(input);

        let actual = part_one(&map, &trail_heads, &trail_ends);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#".....0.
..4321.
..5..2.
..6543.
..7..4.
.98765.
..9...."#,
        6
    )]
    #[case(
        r#"..90..9
...1.98
...2..7
6543456
765.987
896....
987...."#,
        11
    )]
    #[case(
        r#"012345
123456
234567
345678
4.8789
56989."#,
        226
    )]
    #[case(
        r#"89010123
78121874
87430965
96549874
45678903
52019012
01329801
10456732"#,
        71
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let (map, trail_heads, trail_ends) = parse(input);

        let actual = part_two(&map, &trail_heads, &trail_ends);

        assert_eq!(actual, expected);
    }
}
