use std::collections::BTreeMap;

pub type Files = BTreeMap<usize, usize>;
pub type Spaces = Vec<usize>;

pub fn parse(input: &str) -> (Files, Spaces) {
    let line = input.lines().next().expect("must have at least one line");

    let mut files: Files = BTreeMap::new();
    let mut spaces: Spaces = vec![];

    let mut position = 0;
    let mut file_id = 0;
    for (i, c) in line.chars().enumerate() {
        let num_elems = c.to_digit(10).unwrap() as usize;
        let is_file = i % 2 == 0;

        for j in 0..num_elems {
            if is_file {
                files.insert(position + j, file_id);
            } else {
                spaces.push(position + j);
            }
        }

        if is_file {
            file_id += 1;
        }

        position += num_elems;
    }

    (files, spaces)
}

pub fn part_one(files: &BTreeMap<usize, usize>, spaces: &[usize]) -> usize {
    let mut contiguous_files: Files = files.clone();

    for ((file_position, file_id), space_position) in files.iter().rev().zip(spaces) {
        if file_position < space_position {
            break;
        }

        contiguous_files.insert(*space_position, *file_id);
        contiguous_files.remove(file_position);
    }

    let mut checksum = 0;
    for (file_position, file_id) in contiguous_files {
        print!("{file_id}");
        checksum += file_position * file_id
    }

    println!();

    checksum
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[test]
    fn test_parse() {
        let input = "12345";
        let expected_files = BTreeMap::from([
            (0, 0),
            (3, 1),
            (4, 1),
            (5, 1),
            (10, 2),
            (11, 2),
            (12, 2),
            (13, 2),
            (14, 2),
        ]);
        let expected_spaces = vec![1, 2, 6, 7, 8, 9];

        let (actual_files, actual_spaces) = parse(input);

        assert_eq!(actual_files, expected_files);
        assert_eq!(actual_spaces, expected_spaces);
    }

    #[rstest]
    #[case("12345", 60)]
    #[case("2333133121414131402", 1928)]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let (files, spaces) = parse(input);

        let actual = part_one(&files, &spaces);

        assert_eq!(actual, expected);
    }
}
