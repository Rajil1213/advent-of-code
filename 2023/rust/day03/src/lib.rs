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

#[cfg(test)]
mod tests {
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
}
