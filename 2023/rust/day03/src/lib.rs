pub fn compute_parts_sum(matrix: Vec<&str>) -> usize {
    let mut sum = 0;

    for (row, line) in matrix.iter().enumerate() {
        let mut running_number = String::new();
        let mut adjacent = false;

        for (col, ch) in line.chars().enumerate() {
            if ch.is_ascii_digit() {
                running_number.push(ch);

                let adjacent_vals = get_adjacent_vals(row, col, &matrix);
                // already adjacent or new adjacent value
                adjacent = adjacent || contains_special_chars(adjacent_vals);

            // non-number i.e., a symbol has been detected, so check the running number
            } else if !running_number.is_empty() {
                if adjacent {
                    sum += running_number.parse::<usize>().expect("must be a number");
                }

                // reset
                running_number = String::new();
                adjacent = false;
            }
        }
        // if there was a number at the end of a line with adjacent symbols
        if !running_number.is_empty() && adjacent {
            sum += running_number.parse::<usize>().expect("must be a number");
        }
    }

    sum
}

fn get_adjacent_vals(row: usize, col: usize, matrix: &Vec<&str>) -> [char; 8] {
    let mut adjacent_vals: [char; 8] = ['.'; 8];

    let num_rows = matrix.len();
    let num_cols = matrix[row].len();

    let previous_row = row.saturating_sub(1);
    let next_row = if (row + 1) < num_rows {
        row + 1
    } else {
        num_rows - 1
    };

    let previous_col = col.saturating_sub(1);
    let next_col = if (col + 1) < num_cols {
        col + 1
    } else {
        num_cols - 1
    };

    // upper left
    adjacent_vals[0] = matrix[previous_row]
        .chars()
        .nth(previous_col)
        .expect("matrix index must always be valid");
    // upper mid
    adjacent_vals[1] = matrix[previous_row]
        .chars()
        .nth(col)
        .expect("matrix index must always be valid");
    // upper right
    adjacent_vals[2] = matrix[previous_row]
        .chars()
        .nth(next_col)
        .expect("matrix index must always be valid");
    // left
    adjacent_vals[3] = matrix[row]
        .chars()
        .nth(previous_col)
        .expect("matrix index must always be valid");
    // right
    adjacent_vals[4] = matrix[row]
        .chars()
        .nth(next_col)
        .expect("matrix index must always be valid");
    // lower left
    adjacent_vals[5] = matrix[next_row]
        .chars()
        .nth(previous_col)
        .expect("matrix index must always be valid");
    // lower mid
    adjacent_vals[6] = matrix[next_row]
        .chars()
        .nth(col)
        .expect("matrix index must always be valid");
    // upper right
    adjacent_vals[7] = matrix[next_row]
        .chars()
        .nth(next_col)
        .expect("matrix index must always be valid");

    adjacent_vals
}

fn contains_special_chars(adjacent_vals: [char; 8]) -> bool {
    // if all adjacent vals are either `.`'s or ascii digits, there are no special chars
    if adjacent_vals
        .iter()
        .all(|c| *c == '.' || c.is_ascii_digit())
    {
        return false;
    }

    true
}

#[derive(Debug, PartialEq)]
struct NumberSpan {
    start: usize,
    end: usize,
}

#[derive(Debug, PartialEq)]
struct NumInRow {
    num: usize,
    span: NumberSpan,
}

pub fn compute_total_gear_ratio(matrix: Vec<&str>) -> usize {
    const GEAR: char = '*';
    const REQUIRED_ADJACENT_NUMS: usize = 2;

    let mut sum = 0;
    for (row, line) in matrix.iter().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            if ch != GEAR {
                continue;
            }

            let adjacent_nums = find_adjacent_nums((row, col), &matrix);
            if adjacent_nums.len() == REQUIRED_ADJACENT_NUMS {
                sum += adjacent_nums[0] * adjacent_nums[1];
            }
        }
    }

    sum
}

fn find_nums_in_row(row: &str) -> Vec<NumInRow> {
    let mut running_num = String::new();
    let mut nums_in_row = vec![];

    let mut start = 0;
    for (i, ch) in row.chars().enumerate() {
        if ch.is_ascii_digit() {
            if running_num.is_empty() {
                start = i;
            }
            running_num.push(ch);
        } else if !running_num.is_empty() {
            nums_in_row.push(NumInRow {
                num: running_num
                    .parse::<usize>()
                    .expect("must be a valid number"),
                span: NumberSpan { start, end: i - 1 },
            });

            // reset
            running_num = String::new();
        }
    }

    // check again at the end of row
    if !running_num.is_empty() {
        nums_in_row.push(NumInRow {
            num: running_num
                .parse::<usize>()
                .expect("must be a valid number"),
            span: NumberSpan {
                start,
                end: row.len() - 1,
            },
        });
    }

    nums_in_row
}

fn find_adjacent_nums((row, col): (usize, usize), matrix: &Vec<&str>) -> Vec<usize> {
    let previous_row = row.saturating_sub(1);

    let max_row = matrix.len();
    let next_row = if row + 1 < max_row {
        row + 1
    } else {
        max_row - 1
    };

    let mut nums_in_adjacent_rows = find_nums_in_row(matrix[previous_row]);
    nums_in_adjacent_rows.append(&mut find_nums_in_row(matrix[row])); // current
    nums_in_adjacent_rows.append(&mut find_nums_in_row(matrix[next_row]));

    nums_in_adjacent_rows
        .iter()
        .filter_map(|num_in_row| {
            // -1/+1 adjust for diagonals and immediate left/right
            if (num_in_row.span.start.saturating_sub(1)) <= col && col <= (num_in_row.span.end + 1)
            {
                Some(num_in_row.num)
            } else {
                None
            }
        })
        .collect::<Vec<usize>>()
}

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use std::iter::zip;

    use super::*;

    #[test]
    fn computes_sum_correctly() {
        let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";
        let matrix = input.lines().collect::<Vec<&str>>();
        assert_eq!(compute_parts_sum(matrix), 4361);
    }

    #[rstest]
    #[case("..123..#45/..12", vec![NumInRow { num: 123, span: NumberSpan { start: 2, end: 4 }}, NumInRow { num: 45, span: NumberSpan { start: 8, end: 9 }}, NumInRow { num: 12, span: NumberSpan { start: 13, end: 14 }}])]
    fn finds_nums_in_row_correctly(#[case] input: &str, #[case] expected: Vec<NumInRow>) {
        let actual_nums_in_row = find_nums_in_row(input);
        assert_eq!(actual_nums_in_row.len(), expected.len());

        for (actual_val, expected_val) in zip(actual_nums_in_row, expected) {
            assert_eq!(actual_val, expected_val);
        }
    }

    #[test]
    fn computes_adjacent_nums_correctly() {
        let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";
        let matrix = input.lines().collect::<Vec<&str>>();
        assert_eq!(find_adjacent_nums((1, 3), &matrix), vec![467, 35]);
    }
}
