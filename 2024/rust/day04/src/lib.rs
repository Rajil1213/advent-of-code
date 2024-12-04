use std::ops::Not;

pub type Grid = Vec<String>;

pub fn parse(input: &str) -> Grid {
    input.lines().fold(Vec::new(), |mut grid, line| {
        grid.push(line.to_string());

        grid
    })
}

pub fn part_one(grid: &Grid, text_to_find: &str) -> usize {
    let reverse_text = text_to_find.chars().rev().collect::<String>();
    let reverse_text = reverse_text.as_str();

    let grid_size = grid.len();
    let text_size = text_to_find.len();

    let mut count = 0;
    for row in 0..grid_size {
        let row_size = grid[row].len();

        for col in 0..row_size {
            // check horizontal
            let horizontal_slice = &grid[row][col..];
            if horizontal_slice.starts_with(text_to_find)
                || horizontal_slice.starts_with(reverse_text)
            {
                count += 1;
            }

            // check vertical
            let vertical_slice = get_vertical_slice(grid, text_size, row, col);

            if vertical_slice.starts_with(text_to_find) || vertical_slice.starts_with(reverse_text)
            {
                count += 1;
            }

            // check right diagonal
            let right_diagonal_slice = get_right_diagonal(grid, text_size, row, col);

            if right_diagonal_slice.starts_with(text_to_find)
                || right_diagonal_slice.starts_with(reverse_text)
            {
                count += 1;
            }

            // check left diagonal
            let left_diagonal_slice = get_left_diagonal(grid, text_size, row, col);

            if left_diagonal_slice.starts_with(text_to_find)
                || left_diagonal_slice.starts_with(reverse_text)
            {
                count += 1;
            }
        }
    }

    count
}

fn get_vertical_slice(grid: &[String], text_size: usize, row: usize, col: usize) -> String {
    let mut vertical_slice = String::new();

    let grid_size = grid.len();
    let remaining_lines = grid_size - row;

    if remaining_lines >= text_size {
        for k in 0..text_size {
            let chr = grid[row + k].chars().nth(col).unwrap();
            vertical_slice.push(chr);
        }
    }

    vertical_slice
}

fn get_right_diagonal(grid: &[String], text_size: usize, row: usize, col: usize) -> String {
    let mut diagonal_slice = String::new();

    let row_size = grid[row].len();
    let grid_size = grid.len();

    let remaining_chars = row_size - col;
    let remaining_lines = grid_size - row;

    if remaining_chars >= text_size && remaining_lines >= text_size {
        for k in 0..text_size {
            let chr = grid[row + k].chars().nth(col + k).unwrap();
            diagonal_slice.push(chr);
        }
    }

    diagonal_slice
}

fn get_left_diagonal(grid: &[String], text_size: usize, row: usize, col: usize) -> String {
    let mut diagonal_slice = String::new();

    let grid_size = grid.len();
    let remaining_lines = grid_size - row;

    if (col + 1) >= text_size && remaining_lines >= text_size {
        for k in 0..text_size {
            let chr = grid[row + k].chars().nth(col - k).unwrap();
            diagonal_slice.push(chr);
        }
    }

    diagonal_slice
}

pub fn part_two(grid: &Grid, text_to_find: &str) -> usize {
    let reverse_text = text_to_find.chars().rev().collect::<String>();
    let reverse_text = reverse_text.as_str();

    let grid_size = grid.len();
    let text_size = text_to_find.len();

    let mut count = 0;
    for row in 0..grid_size {
        let row_size = grid[row].len();
        let mut valid_right_diagonals: Vec<usize> = vec![];

        for col in 0..row_size {
            // check right diagonal
            let right_diagonal_slice = get_right_diagonal(grid, text_size, row, col);

            if right_diagonal_slice.starts_with(text_to_find)
                || right_diagonal_slice.starts_with(reverse_text)
            {
                valid_right_diagonals.push(col);
            }

            let right_diagonal_index: isize = col as isize - (text_size as isize - 1);
            let should_check_left = right_diagonal_index >= 0
                && valid_right_diagonals.contains(&(right_diagonal_index as usize));

            if should_check_left.not() {
                continue;
            }

            // check left diagonal
            let left_diagonal_slice = get_left_diagonal(grid, text_size, row, col);

            if left_diagonal_slice.starts_with(text_to_find)
                || left_diagonal_slice.starts_with(reverse_text)
            {
                count += 1;
            }
        }
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        r#"..X...
.SAMX.
.A..A.
XMAS.S
.X...."#,
        4
    )]
    #[case(
        r#"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"#,
        18
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let parsed_text = parse(input);
        let actual = part_one(&parsed_text, "XMAS");

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"M.S
.A.
M.S"#,
        1
    )]
    #[case(
        r#"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"#,
        9
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let parsed_text = parse(input);
        let actual = part_two(&parsed_text, "MAS");

        assert_eq!(actual, expected);
    }
}
