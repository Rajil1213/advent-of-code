use std::collections::BTreeMap;

pub type Files = BTreeMap<usize, usize>;
pub type Spaces = Vec<usize>;

pub type SpaceBlocks = BTreeMap<usize, usize>; // starting position -> count
pub type FileBlocks = BTreeMap<usize, (usize, usize)>; // starting_position -> (file_id, count)

pub fn parse(input: &str) -> (Files, Spaces, FileBlocks, SpaceBlocks) {
    let line = input.lines().next().expect("must have at least one line");

    let mut files: Files = BTreeMap::new();
    let mut spaces: Spaces = vec![];

    let mut file_blocks: FileBlocks = BTreeMap::new();
    let mut space_blocks: SpaceBlocks = BTreeMap::new();

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
            file_blocks.insert(position, (file_id, num_elems));
            file_id += 1;
        } else {
            space_blocks.insert(position, num_elems);
        }

        position += num_elems;
    }

    (files, spaces, file_blocks, space_blocks)
}

pub fn part_one(files: &Files, spaces: &Spaces) -> usize {
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
        checksum += file_position * file_id
    }

    checksum
}

pub fn part_two(file_blocks: &FileBlocks, space_blocks: &SpaceBlocks) -> usize {
    let mut consolidated_files: FileBlocks = file_blocks.clone();

    let mut space_counts = space_blocks.iter();

    loop {
        let space_counts = space_counts.next();
        if space_counts.is_none() {
            break;
        }

        let (space_position, space_to_fill) = space_counts.unwrap();
        let (mut space_position, mut space_to_fill) = (*space_position, *space_to_fill);

        let updated_consolidated_files = consolidated_files
            .iter()
            .filter_map(|(file_pos, value)| {
                if *file_pos > space_position {
                    Some((*file_pos, *value))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        if updated_consolidated_files.is_empty() {
            break;
        }

        for (file_position, (file_id, file_count)) in updated_consolidated_files.into_iter().rev() {
            if file_position < space_position {
                break;
            }

            if file_count == space_to_fill {
                consolidated_files.insert(space_position, (file_id, file_count));
                consolidated_files.remove(&file_position);

                break;
            }

            if file_count < space_to_fill {
                consolidated_files.insert(space_position, (file_id, file_count));
                consolidated_files.remove(&file_position);

                space_to_fill -= file_count;
                space_position += file_count;
            }
        }
    }

    let mut checksum = 0;
    for (file_position, (file_id, file_count)) in consolidated_files {
        for i in 0..file_count {
            checksum += (file_position + i) * file_id;
        }
    }

    checksum
}

pub type SpaceBlock = BTreeMap<usize, usize>; // starting position -> count
pub type FileBlock = BTreeMap<usize, (usize, usize)>; // starting_position -> (file_id, count)

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

        let expected_file_blocks = BTreeMap::from([(0, (0, 1)), (3, (1, 3)), (10, (2, 5))]);
        let expected_space_blocks = BTreeMap::from([(1, 2), (6, 4)]);

        let (actual_files, actual_spaces, actual_file_blocks, actual_space_blocks) = parse(input);

        assert_eq!(actual_files, expected_files);
        assert_eq!(actual_spaces, expected_spaces);

        assert_eq!(actual_file_blocks, expected_file_blocks);
        assert_eq!(actual_space_blocks, expected_space_blocks);
    }

    #[rstest]
    #[case("12345", 60)]
    #[case("2333133121414131402", 1928)]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let (files, spaces, _, _) = parse(input);

        let actual = part_one(&files, &spaces);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("2333133121414131402", 2858)]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let (_, _, file_blocks, space_blocks) = parse(input);

        let actual = part_two(&file_blocks, &space_blocks);

        assert_eq!(actual, expected);
    }
}
