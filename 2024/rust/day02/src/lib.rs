pub fn part_one(input: Vec<Vec<i64>>) -> i64 {
    input.into_iter().filter(|levels| is_safe(levels)).count() as i64
}

pub fn part_two(input: Vec<Vec<i64>>) -> i64 {
    input
        .into_iter()
        .filter(|levels| is_safe(levels) || sublists(levels).iter().any(|sublist| is_safe(sublist)))
        .count() as i64
}

fn is_safe(levels: &[i64]) -> bool {
    let expected_diff = levels[1] - levels[0];

    levels
        .iter()
        .enumerate()
        .skip(1)
        .all(|(i, _)| match levels[i] - levels[i - 1] {
            (-3..=-1) => (-3..=-1).contains(&expected_diff),
            (1..=3) => (1..=3).contains(&expected_diff),
            _ => false,
        })
}

fn sublists(list: &[i64]) -> Vec<Vec<i64>> {
    list.iter()
        .enumerate()
        .map(|(idx_to_skip, _)| {
            list.iter()
                .enumerate()
                .filter(|(idx_to_keep, _)| *idx_to_keep != idx_to_skip)
                .map(|(_, elem)| *elem)
                .collect::<Vec<i64>>()
        })
        .collect()
}

pub fn parse(input: String) -> Vec<Vec<i64>> {
    input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num_str| num_str.parse::<i64>().unwrap())
                .collect::<Vec<i64>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(r#"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"#, vec!( vec!( 7, 6, 4, 2, 1 ), vec!( 1, 2, 7, 8, 9 ), vec!( 9, 7, 6, 2, 1 ), vec!( 1, 3, 2, 4, 5 ), vec!( 8, 6, 4, 4, 1 ), vec!( 1, 3, 6, 7, 9 ) ) )]
    fn test_parse(#[case] input: String, #[case] expected: Vec<Vec<i64>>) {
        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(vec!( vec!( 7, 6, 4, 2, 1 ), vec!( 1, 2, 7, 8, 9 ), vec!( 9, 7, 6, 2, 1 ), vec!( 1, 3, 2, 4, 5 ), vec!( 8, 6, 4, 4, 1 ), vec!( 1, 3, 6, 7, 9 ) ), 2)]
    fn test_part_one(#[case] input: Vec<Vec<i64>>, #[case] expected: i64) {
        let actual = part_one(input);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(vec!( vec!( 7, 6, 4, 2, 1 ), vec!( 1, 2, 7, 8, 9 ), vec!( 9, 7, 6, 2, 1 ), vec!( 1, 3, 2, 4, 5 ), vec!( 8, 6, 4, 4, 1 ), vec!( 1, 3, 6, 7, 9 ) ), 4)]
    fn test_part_two(#[case] input: Vec<Vec<i64>>, #[case] expected: i64) {
        let actual = part_two(input);

        assert_eq!(actual, expected);
    }
}
