#[derive(Debug, Clone)]
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
}

type RockLine = Vec<Option<Rock>>;
type RockPattern = Vec<RockLine>;

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
}
