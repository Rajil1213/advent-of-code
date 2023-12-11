const GALAXY: char = '#';
const SPACE: char = '.';

pub fn load_image(input: &str) -> Vec<&str> {
    let mut galaxy: Vec<&str> = vec![];

    for line in input.lines() {
        galaxy.push(line);
    }

    galaxy
}

pub fn expand_space(image: &[&str], factor: usize) -> Vec<String> {
    let mut transposed_image: Vec<String> = vec![];

    let num_col = image
        .first()
        .expect("galaxy must contain at least one row")
        .len();

    let mut col_cnt = 0;
    while col_cnt < num_col {
        let mut transposed_row: String = String::new();
        for line in image.iter() {
            transposed_row.push(line.chars().nth(col_cnt).expect("column must exist"));
        }
        transposed_image.push(transposed_row);
        col_cnt += 1;
    }

    let mut expanded_space: Vec<String> = vec![];

    for row in image.iter() {
        let mut expanded_row: String = String::new();
        // each value in transposed_galaxy is a character of the `row` in the original
        for (char_num, col) in transposed_image.iter().enumerate() {
            let chr = row.chars().nth(char_num).expect("character must exist");
            expanded_row.push(chr);
            // push again if the col only has `.`
            if col.chars().all(|c| c == SPACE) {
                for _ in 0..factor {
                    expanded_row.push(chr);
                }
            }
        }
        expanded_space.push(expanded_row.clone());
        // push again if the row has only `.`
        if expanded_row.chars().all(|c| c == SPACE) {
            for _ in 0..factor {
                expanded_space.push(expanded_row.clone());
            }
        }
    }

    expanded_space
}

pub fn expanded_locations(image: &[&str]) -> (Vec<usize>, Vec<usize>) {
    let mut transposed_image: Vec<String> = vec![];

    let num_col = image
        .first()
        .expect("galaxy must contain at least one row")
        .len();

    let mut col_cnt = 0;
    while col_cnt < num_col {
        let mut transposed_row: String = String::new();
        for line in image.iter() {
            transposed_row.push(line.chars().nth(col_cnt).expect("column must exist"));
        }
        transposed_image.push(transposed_row);
        col_cnt += 1;
    }

    let mut expanded_cols: Vec<usize> = vec![];
    // each value in transposed_galaxy is a character of the `row` in the original
    for (char_num, col) in transposed_image.iter().enumerate() {
        if col.chars().all(|c| c == SPACE) {
            expanded_cols.push(char_num);
        }
    }

    let mut expanded_rows: Vec<usize> = vec![];
    for (row_no, row) in image.iter().enumerate() {
        if row.chars().all(|c| c == SPACE) {
            expanded_rows.push(row_no);
        }
    }

    (expanded_rows, expanded_cols)
}

pub fn find_galaxies(image: &[&str]) -> Vec<(usize, usize)> {
    let mut galaxies: Vec<(usize, usize)> = vec![];
    for (row_no, row) in image.iter().enumerate() {
        for (col_no, chr) in row.chars().enumerate() {
            if chr == GALAXY {
                galaxies.push((row_no, col_no));
            }
        }
    }

    galaxies
}

pub fn find_distance_between_pairs(
    galaxy_positions: Vec<(usize, usize)>,
    expanded_spaces: (Vec<usize>, Vec<usize>),
    expansion_factor: usize,
) -> Vec<usize> {
    let mut distances: Vec<usize> = vec![];
    for (i, (x, y)) in galaxy_positions.iter().enumerate() {
        for (next_x, next_y) in galaxy_positions[i + 1..].iter() {
            let mut abs_distance = next_x.abs_diff(*x) + next_y.abs_diff(*y);
            // dbg!(abs_distance);

            let mut num_crossings: usize = 0;
            for expanded_row in expanded_spaces.0.iter() {
                if expanded_row.gt(x) && expanded_row.lt(next_x) {
                    num_crossings += 1;
                }
            }

            for expanded_col in expanded_spaces.1.iter() {
                if (expanded_col.gt(y) && expanded_col.lt(next_y))
                    || (expanded_col.gt(next_y) && expanded_col.lt(y))
                {
                    num_crossings += 1;
                }
            }
            // dbg!(x, y, next_x, next_y, num_crossings);

            abs_distance += (num_crossings * expansion_factor).saturating_sub(num_crossings);
            // dbg!(abs_distance);

            distances.push(abs_distance);
        }
    }

    distances
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expands_galaxy_correctly() {
        let input: &str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";
        let expected_output: &str = "....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#.......";
        let image = load_image(input);
        let expanded_image = expand_space(&image[..], 1);

        let mut output: String = String::new();
        for row in expanded_image {
            output.push_str(&row);
            output.push('\n');
        }
        output.truncate(output.len() - 1); // remove last character i.e., '\n'

        assert_eq!(output, expected_output.to_string());
    }

    #[test]
    fn finds_galaxies_correctly() {
        let input: &str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";
        let image = load_image(input);
        let expanded_image = expand_space(&image[..], 1);

        let expected_galaxies = vec![
            (0, 4),
            (1, 9),
            (2, 0),
            (5, 8),
            (6, 1),
            (7, 12),
            (10, 9),
            (11, 0),
            (11, 5),
        ];
        let galaxies = find_galaxies(
            &expanded_image
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<&str>>(),
        );
        assert_eq!(galaxies, expected_galaxies);
    }

    #[test]
    fn calculates_sum_of_distances_correctly() {
        let input: &str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";
        let image = load_image(input);
        let expanded_locations = expanded_locations(&image);
        // dbg!(&expanded_locations);
        let galaxies = find_galaxies(&image);

        let expected_sum = 374;
        let distance_pairs = find_distance_between_pairs(galaxies, expanded_locations, 2);
        assert_eq!(distance_pairs.iter().sum::<usize>(), expected_sum);
    }
}
