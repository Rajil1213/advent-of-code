pub const TOTAL_CYCLES: usize = 1000000000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rock {
    Round,
    Square,
}

impl Rock {
    pub fn from_char(ch: char) -> Option<Self> {
        match ch {
            'O' => Some(Self::Round),
            '#' => Some(Self::Square),
            '.' => None,
            _ => panic!("unknown object detected: {ch}"),
        }
    }

    pub fn to_char(rock: &Option<Self>) -> char {
        match rock {
            Some(Self::Round) => 'O',
            Some(Self::Square) => '#',
            None => '.',
        }
    }
}

pub type RockLine = Vec<Option<Rock>>;
pub type RockPattern = Vec<RockLine>;

pub fn parse(contents: &str) -> Vec<RockPattern> {
    let mut output: Vec<RockPattern> = vec![];

    let mut rock_pattern: RockPattern = vec![];
    for line in contents.lines() {
        if line.trim().is_empty() {
            output.push(rock_pattern);
            rock_pattern = vec![];
            continue;
        }

        let mut rock_line: RockLine = vec![];
        for chr in line.chars() {
            rock_line.push(Rock::from_char(chr));
        }
        rock_pattern.push(rock_line);
    }

    if !rock_pattern.is_empty() {
        output.push(rock_pattern);
    }

    output
}

pub fn transpose(pattern: &RockPattern) -> RockPattern {
    let mut transposed_rock_pattern: RockPattern = vec![];

    let col_count = pattern[0].len();
    let mut col_num = 0;

    while col_num < col_count {
        let mut transposed_rock_line: RockLine = vec![];
        for line in pattern {
            transposed_rock_line.push(line[col_num].clone());
        }
        col_num += 1;
        transposed_rock_pattern.push(transposed_rock_line);
    }

    transposed_rock_pattern
}

pub fn rock_roll_sum(transposed_pattern: &RockPattern) -> usize {
    let mut sum = 0;

    for rock_line in transposed_pattern {
        let total_height = rock_line.len();
        let mut height = total_height;
        for (i, rock) in rock_line.iter().enumerate() {
            if rock.is_some() {
                let rock_type = rock.clone().unwrap();

                match rock_type {
                    Rock::Round => sum += height,
                    Rock::Square => height = total_height - i,
                }
                height -= 1;
            }
        }
    }
    sum
}

/// This function cycles the `pattern` repeatedly till the next pattern matches any of the paterns
/// that have been seen so far.
///
/// # Returns
///
/// 1. The vector of all patterns that have been explored till seeing a repetition.
/// 2. The offset point i.e., the index of the pattern that was seen again.
pub fn cycle_length(pattern: &RockPattern) -> (Vec<RockPattern>, usize) {
    const MAX_PATTERNS_TO_CHECK: usize = 10000;

    let mut patterns: Vec<RockPattern> = vec![];

    let mut next = spin_cycle(pattern);
    patterns.push(next.clone());

    loop {
        let cycle_count = patterns.len();

        next = spin_cycle(&next);
        for (i, pat) in patterns.iter().enumerate() {
            if pat.eq(&next) {
                return (patterns, i);
            }
        }

        patterns.push(next.clone());

        if cycle_count > MAX_PATTERNS_TO_CHECK {
            panic!("Checked {MAX_PATTERNS_TO_CHECK} patterns without any recurrence");
        }
    }
}

pub fn calculate_load_after_cycles(
    total_cycles: usize,
    patterns: &[RockPattern],
    offset: usize,
) -> usize {
    let remaining_cycles = total_cycles - offset;
    let surplus = remaining_cycles % (patterns.len() - offset);

    let index_at_surplus = if surplus == 0 {
        patterns.len() - 1 // if no surplus, get the last value in the repeating pattern
    } else {
        offset + surplus - 1 // if there is a surplus, start counting from the offset point till
                             // the surplus
    };

    calculate_load(&patterns[index_at_surplus])
}

pub fn calculate_load(pattern: &RockPattern) -> usize {
    let total_height = pattern.len();
    let mut total_load = 0;
    for (row, rock_line) in pattern.iter().enumerate() {
        let round_count: usize = rock_line
            .iter()
            .filter_map(|r| {
                if r.is_some() && r.clone().unwrap() == Rock::Round {
                    Some(1)
                } else {
                    None
                }
            })
            .sum();
        total_load += round_count * (total_height - row);
    }

    total_load
}

pub fn spin_cycle(pattern: &RockPattern) -> RockPattern {
    roll_east(&roll_south(&roll_west(&roll_north(pattern))))
}

pub fn roll_north(pat: &RockPattern) -> RockPattern {
    let height = pat.len();
    let width = pat[0].len();
    let mut rolled_pattern: RockPattern = vec![vec![None; width]; height];

    for col in 0..width {
        let mut rolled_row = 0;
        for (row, rock_line) in pat.iter().enumerate() {
            if rock_line[col].is_some() {
                let element = rock_line[col].clone().unwrap();
                if element == Rock::Square {
                    rolled_row = row;
                }
                rolled_pattern[rolled_row][col] = Some(element);
                rolled_row += 1;
            }
        }
    }

    rolled_pattern
}

pub fn roll_south(pat: &RockPattern) -> RockPattern {
    let height = pat.len();
    let width = pat[0].len();
    let mut rolled_pattern: RockPattern = vec![vec![None; width]; height];

    for col in 0..width {
        let mut rolled_row = height - 1;
        for row in (0..height).rev() {
            if pat[row][col].is_some() {
                let element = pat[row][col].clone().unwrap();
                if element == Rock::Square {
                    rolled_row = row;
                }
                rolled_pattern[rolled_row][col] = Some(element);
                rolled_row = rolled_row.saturating_sub(1);
            }
        }
    }

    rolled_pattern
}

pub fn roll_west(pat: &RockPattern) -> RockPattern {
    let height = pat.len();
    let width = pat[0].len();
    let mut rolled_pattern: RockPattern = vec![vec![None; width]; height];

    for row in 0..height {
        let mut rolled_col = 0;
        for col in 0..width {
            if pat[row][col].is_some() {
                let element = pat[row][col].clone().unwrap();
                if element == Rock::Square {
                    rolled_col = col;
                }
                rolled_pattern[row][rolled_col] = Some(element);
                rolled_col += 1;
            }
        }
    }

    rolled_pattern
}

pub fn roll_east(pat: &RockPattern) -> RockPattern {
    let height = pat.len();
    let width = pat[0].len();
    let mut rolled_pattern: RockPattern = vec![vec![None; width]; height];

    for row in 0..height {
        let mut rolled_col = width - 1;
        for col in (0..width).rev() {
            if pat[row][col].is_some() {
                let element = pat[row][col].clone().unwrap();
                if element == Rock::Square {
                    rolled_col = col;
                }
                rolled_pattern[row][rolled_col] = Some(element);
                rolled_col = rolled_col.saturating_sub(1);
            }
        }
    }

    rolled_pattern
}

pub fn pattern_to_string(pat: &RockPattern) -> String {
    let mut output = String::new();
    let last_index = pat.len() - 1;
    for (i, rock_line) in pat.iter().enumerate() {
        for rock_type in rock_line {
            output.push(Rock::to_char(rock_type));
        }

        if i < last_index {
            output.push('\n');
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_sum_correctly() {
        let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";
        let patterns = parse(input);
        let transposed_pattern = transpose(&patterns[0]);

        assert_eq!(rock_roll_sum(&transposed_pattern), 136);
    }

    #[test]
    fn rolls_north_correctly() {
        let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";
        let expected: &str = "OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....";
        let patterns = parse(input);
        let north_rolled = roll_north(&patterns[0]);

        assert_eq!(pattern_to_string(&north_rolled), expected.to_string());
    }

    #[test]
    fn rolls_west_correctly() {
        let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";
        let expected = "O....#....
OOO.#....#
.....##...
OO.#OO....
OO......#.
O.#O...#.#
O....#OO..
O.........
#....###..
#OO..#....";

        let patterns = parse(input);
        let west_rolled = roll_west(&patterns[0]);

        assert_eq!(pattern_to_string(&west_rolled), expected.to_string());
    }

    #[test]
    fn rolls_south_correctly() {
        let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";
        let expected: &str = ".....#....
....#....#
...O.##...
...#......
O.O....O#O
O.#..O.#.#
O....#....
OO....OO..
#OO..###..
#OO.O#...O";

        let patterns = parse(input);
        let south_rolled = roll_south(&patterns[0]);

        assert_eq!(pattern_to_string(&south_rolled), expected.to_string());
    }

    #[test]
    fn rolls_east_correctly() {
        let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";
        let expected: &str = "....O#....
.OOO#....#
.....##...
.OO#....OO
......OO#.
.O#...O#.#
....O#..OO
.........O
#....###..
#..OO#....";

        let patterns = parse(input);
        let east_rolled = roll_east(&patterns[0]);

        assert_eq!(pattern_to_string(&east_rolled), expected.to_string());
    }

    #[test]
    fn finds_cycle_length() {
        let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";

        let patterns = parse(input);
        let (min_patterns, offset) = cycle_length(&patterns[0]);
        let load = calculate_load_after_cycles(TOTAL_CYCLES, &min_patterns, offset);
        assert_eq!(load, 64);
    }
}
