use std::{collections::HashSet, ops::Not};

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

impl Position {
    pub fn next_positions(&self, max_position: &Position) -> Vec<Position> {
        let mut next_positions = Vec::with_capacity(4);

        // up
        if self.x != 0 {
            next_positions.push((self.x - 1, self.y).into());
        }

        let right = self.x + 1;
        if right < max_position.x {
            next_positions.push((right, self.y).into());
        }

        // left
        if self.y != 0 {
            next_positions.push((self.x, self.y - 1).into());
        }

        let down = self.y + 1;
        if down < max_position.y {
            next_positions.push((self.x, down).into());
        }

        next_positions
    }
}

pub type Plot = Vec<Vec<char>>;

pub fn parse(input: &str) -> Plot {
    input.lines().fold(Vec::new(), |mut plot, row| {
        let row = row.chars().fold(Vec::new(), |mut row_plot, col_val| {
            row_plot.push(col_val);

            row_plot
        });

        plot.push(row);

        plot
    })
}

pub fn part_one(plot: &Plot) -> usize {
    let mut total_cost: usize = 0;

    let mut regions: Vec<HashSet<Position>> = vec![];

    for (row_num, row) in plot.iter().enumerate() {
        for (col_num, col) in row.iter().enumerate() {
            let starting_position: Position = (row_num, col_num).into();
            if regions.iter().any(|r| r.contains(&starting_position)) {
                continue;
            }

            let mut region: HashSet<Position> = HashSet::from([starting_position]);
            find_region(&starting_position, plot, &mut region);

            let area = region.len();
            let perimeter = get_perimeter(col, &region, plot);

            total_cost += area * perimeter;

            regions.push(region);
        }
    }

    total_cost
}

pub fn part_two(plot: &Plot) -> usize {
    let mut total_cost: usize = 0;

    let mut regions: Vec<HashSet<Position>> = vec![];

    for (row_num, row) in plot.iter().enumerate() {
        for (col_num, col) in row.iter().enumerate() {
            let starting_position: Position = (row_num, col_num).into();
            if regions.iter().any(|r| r.contains(&starting_position)) {
                continue;
            }

            let mut region: HashSet<Position> = HashSet::from([starting_position]);
            find_region(&starting_position, plot, &mut region);

            let area = region.len();
            let sides = get_sides(col, &region, plot);

            total_cost += area * sides;

            regions.push(region);
        }
    }

    total_cost
}

fn find_region(starting_position: &Position, plot: &Plot, region: &mut HashSet<Position>) {
    let max_position = (plot.len(), plot[0].len()).into();
    let next_positions = starting_position.next_positions(&max_position);
    let label = plot[starting_position.x][starting_position.y];

    for next_position in next_positions {
        if plot[next_position.x][next_position.y] != label {
            continue;
        }

        // cycle
        if region.insert(next_position).not() {
            continue;
        }

        find_region(&next_position, plot, region);
    }
}

fn get_perimeter(label: &char, region: &HashSet<Position>, plot: &Plot) -> usize {
    let mut perimeter = 0;

    for position in region {
        let has_up = position.x != 0 && plot[position.x - 1][position.y] == *label;
        let has_left = position.y != 0 && plot[position.x][position.y - 1] == *label;

        perimeter += match (has_left, has_up) {
            (true, true) => 0,
            (true, false) => 2,
            (false, true) => 2,
            (false, false) => 4,
        }
    }

    perimeter
}

fn get_sides(label: &char, region: &HashSet<Position>, plot: &Plot) -> usize {
    let max_position: Position = (plot.len(), plot[0].len()).into();

    let mut num_sides: usize = 0;

    for (row_num, row) in plot.iter().enumerate() {
        let mut encountered_up: bool = false;
        let mut encountered_down: bool = false;

        for (col_num, _col) in row.iter().enumerate() {
            let position: Position = (row_num, col_num).into();

            if region.contains(&position).not() {
                encountered_up = false;
                encountered_down = false;
                continue;
            }

            let has_up = position.x != 0 && plot[position.x - 1][position.y] == *label;
            let has_down =
                position.x + 1 < max_position.x && plot[position.x + 1][position.y] == *label;

            let sides = match (encountered_up, encountered_down) {
                (true, true) => 0, // add nothing because the upper and lower sides are accounted
                (true, false) => {
                    if has_down {
                        0
                    } else {
                        1
                    }
                }
                (false, true) => {
                    if has_up {
                        0
                    } else {
                        1
                    }
                }
                (false, false) => match (has_up, has_down) {
                    (true, true) => 0,
                    (false, false) => 2,
                    _ => 1,
                },
            };

            // if node has an upper node, it breaks the upper horizontal side
            encountered_up = !has_up;
            // if node has a lower node, it breaks the lower horizontal side
            encountered_down = !has_down;

            num_sides += sides;
        }
    }

    for col_num in 0..max_position.y {
        let mut encountered_left: bool = false;
        let mut encountered_right: bool = false;

        for row_num in 0..max_position.x {
            let position: Position = (row_num, col_num).into();

            if region.contains(&position).not() {
                encountered_left = false;
                encountered_right = false;
                continue;
            }

            let has_left = position.y != 0 && plot[position.x][position.y - 1] == *label;
            let has_right =
                position.y + 1 < max_position.y && plot[position.x][position.y + 1] == *label;

            let sides = match (encountered_left, encountered_right) {
                (true, true) => 0, // add nothing because the upper and lower sides are accounted
                (true, false) => {
                    if has_right {
                        0
                    } else {
                        1
                    }
                }
                (false, true) => {
                    if has_left {
                        0
                    } else {
                        1
                    }
                }
                (false, false) => match (has_left, has_right) {
                    (true, true) => 0,
                    (false, false) => 2,
                    _ => 1,
                },
            };

            encountered_right = !has_right;
            encountered_left = !has_left;

            num_sides += sides;
        }
    }

    num_sides
}

#[cfg(test)]
mod tests {
    use crate::part_one;

    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        r#"AAAA
BBCD
BBCC
EEEC"#,
        140
    )]
    #[case(
        r#"OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"#,
        772
    )]
    #[case(
        r#"EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"#,
        692
    )]
    #[case(
        r#"AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"#,
        1184
    )]
    #[case(
        r#"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"#,
        1930
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let plot = parse(input);

        let actual = part_one(&plot);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"AAAA
BBCD
BBCC
EEEC"#,
        80
    )]
    #[case(
        r#"OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"#,
        436
    )]
    #[case(
        r#"EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"#,
        236
    )]
    #[case(
        r#"AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"#,
        368
    )]
    #[case(
        r#"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"#,
        1206
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let plot = parse(input);

        let actual = part_two(&plot);

        assert_eq!(actual, expected);
    }
}
