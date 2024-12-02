pub fn part_one(input: (Vec<u64>, Vec<u64>)) -> u64 {
    let (mut list1, mut list2) = input;

    list1.sort();
    list2.sort();

    list1
        .into_iter()
        .zip(list2)
        .map(|(elem1, elem2)| elem1.abs_diff(elem2))
        .sum()
}

pub fn part_two(input: (Vec<u64>, Vec<u64>)) -> u64 {
    let (list1, list2) = input;

    list1
        .iter()
        .map(|elem1| elem1 * (list2.iter().filter(|value| *value == elem1).count() as u64))
        .sum()
}

pub fn parse_input(input: String) -> (Vec<u64>, Vec<u64>) {
    input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num_str| num_str.parse::<u64>().unwrap())
        })
        .fold(
            (Vec::new(), Vec::new()),
            |(mut list1, mut list2), mut values| {
                list1.push(values.next().unwrap());
                list2.push(values.next().unwrap());

                (list1, list2)
            },
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        r#"3   4
4   3
2   5
1   3
3   9
3   3"#,
        (vec![3, 4, 2, 1, 3, 3], vec![4, 3, 5, 3, 9, 3])
    )]
    fn parses_correctly(#[case] input: String, #[case] expected: (Vec<u64>, Vec<u64>)) {
        let actual = parse_input(input);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case((vec![3, 4, 2, 1, 3, 3], vec![4, 3, 5, 3, 9, 3]), 11)]
    fn part_one_works(#[case] input: (Vec<u64>, Vec<u64>), #[case] expected: u64) {
        let actual = part_one(input);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case((vec![3, 4, 2, 1, 3, 3], vec![4, 3, 5, 3, 9, 3]), 31)]
    fn part_two_works(#[case] input: (Vec<u64>, Vec<u64>), #[case] expected: u64) {
        let actual = part_two(input);

        assert_eq!(actual, expected);
    }
}
