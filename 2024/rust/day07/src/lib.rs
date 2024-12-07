use std::{collections::HashSet, fmt::Display};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Entry {
    pub test_value: usize,
    pub values: Vec<usize>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Operation {
    Add,
    Multiply,
    Concatenate,
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Add => write!(f, "+"),
            Operation::Multiply => write!(f, "*"),
            Operation::Concatenate => write!(f, "||"),
        }
    }
}

impl Operation {
    pub fn operate(&self, lhs: usize, rhs: usize) -> usize {
        match self {
            Operation::Add => lhs + rhs,
            Operation::Multiply => lhs * rhs,
            Operation::Concatenate => format!("{lhs}{rhs}").parse::<usize>().unwrap(),
        }
    }
}

pub fn parse(input: &str) -> Vec<Entry> {
    input
        .lines()
        .map(|line| {
            let values = line.split(":").collect::<Vec<&str>>();
            let target: usize = values[0].parse().unwrap();

            let values = values[1]
                .split_whitespace()
                .map(|v| v.parse::<usize>().unwrap())
                .collect::<Vec<_>>();

            Entry {
                test_value: target,
                values,
            }
        })
        .collect()
}

pub fn part_one(entries: &[Entry]) -> usize {
    entries
        .iter()
        .filter_map(|entry| {
            if let (true, _) = dfs(
                entry.test_value,
                &HashSet::from([Operation::Add, Operation::Multiply]),
                &entry.values,
                String::from("0"),
                0,
            ) {
                Some(entry.test_value)
            } else {
                None
            }
        })
        .sum()
}

pub fn part_two(entries: &[Entry]) -> usize {
    entries
        .iter()
        .filter_map(|entry| {
            if let (true, _) = dfs(
                entry.test_value,
                &HashSet::from([Operation::Add, Operation::Multiply, Operation::Concatenate]),
                &entry.values,
                String::from("0"),
                0,
            ) {
                Some(entry.test_value)
            } else {
                None
            }
        })
        .sum()
}

pub fn dfs(
    target: usize,
    operations: &HashSet<Operation>,
    values: &[usize],
    expr: String,
    cur_total: usize,
) -> (bool, String) {
    if values.is_empty() {
        return (false, String::new());
    }

    let value = values.first().unwrap();

    for operation in operations {
        let new_expr = format!("({expr}) {operation} {value}");
        let new_total = operation.operate(cur_total, *value);

        if values.len() == 1 && new_total == target {
            return (true, new_expr);
        }

        if new_total <= target {
            match dfs(target, operations, &values[1..], new_expr, new_total) {
                (true, expr) => return (true, expr),
                _ => continue,
            }
        }
    }

    (false, String::new())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(( 190, vec![10, 19]), true)]
    #[case(( 3267, vec![81, 40, 27] ), true)]
    #[case(( 83, vec![17, 5] ), false)]
    #[case(( 21037, vec![9, 7, 18, 13] ), false)]
    #[case(( 292, vec![11, 6, 16, 20] ), true)]
    fn test_dfs(#[case] input: (usize, Vec<usize>), #[case] expected: bool) {
        let (target, values) = input;

        let (actual, _) = dfs(
            target,
            &HashSet::from([Operation::Add, Operation::Multiply]),
            &values,
            String::from("0"),
            0,
        );

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case((156, vec![15, 6]), true)]
    #[case((1951, vec![4, 33, 4, 7, 120, 7, 4, 3, 1, 8, 2, 8]), true)]
    #[case((7290, vec![6, 8, 6, 15, 1]), true)]
    fn test_dfs_concatenate(#[case] input: (usize, Vec<usize>), #[case] expected: bool) {
        let (target, values) = input;

        let (actual, _expr) = dfs(
            target,
            &HashSet::from([Operation::Add, Operation::Multiply, Operation::Concatenate]),
            &values,
            String::from("0"),
            0,
        );

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"#,
        3749
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let entries = parse(input);

        let actual = part_one(&entries);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"#,
        11387
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let entries = parse(input);

        let actual = part_two(&entries);

        assert_eq!(actual, expected);
    }
}
