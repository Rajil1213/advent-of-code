use std::usize;

const GALAXY: char = '#';
const SPACE: char = '.';

pub fn load_image(input: &str) -> Vec<&str> {
    let mut galaxy: Vec<&str> = vec![];

    for line in input.lines() {
        galaxy.push(line);
    }

    galaxy
}

pub fn expand_space(image: &[&str]) -> Vec<String> {
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
                expanded_row.push(chr);
            }
        }
        expanded_space.push(expanded_row.clone());
        // push again if the row has only `.`
        if expanded_row.chars().all(|c| c == SPACE) {
            expanded_space.push(expanded_row);
        }
    }

    expanded_space
}

pub fn find_galaxies(image: &[String]) -> Vec<(usize, usize)> {
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

pub fn find_distance_between_pairs(galaxy_positions: Vec<(usize, usize)>) -> Vec<usize> {
    let mut distances: Vec<usize> = vec![];
    for (i, (x, y)) in galaxy_positions.iter().enumerate() {
        for (next_x, next_y) in galaxy_positions[i + 1..].iter() {
            distances.push(next_x.abs_diff(*x) + next_y.abs_diff(*y));
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
        let expanded_image = expand_space(&image[..]);

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
        let expanded_image = expand_space(&image[..]);

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
        let galaxies = find_galaxies(&expanded_image);
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
        let expanded_image = expand_space(&image[..]);
        let galaxies = find_galaxies(&expanded_image);

        let expected_sum = 374;
        let distance_pairs = find_distance_between_pairs(galaxies);
        assert_eq!(distance_pairs.iter().sum::<usize>(), expected_sum);
    }
}
