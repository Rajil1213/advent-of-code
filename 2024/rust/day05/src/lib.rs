use std::{
    collections::{HashMap, HashSet},
    ops::Not,
};

pub type Succeeding = HashSet<u64>;
pub type Preceding = u64;
pub type Order = HashMap<Preceding, Succeeding>;

pub type Entry = Vec<u64>;
pub type Entries = Vec<Entry>;

pub fn parse(input: &str) -> (Order, Entries) {
    let mut lines = input.lines();

    let mut order: Order = HashMap::new();

    loop {
        let line = lines.next().unwrap();

        if line.is_empty() {
            break;
        }

        let values = line
            .split('|')
            .map(|v| v.parse::<u64>().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(values.len(), 2);

        order
            .entry(values[0])
            .and_modify(|v| {
                v.insert(values[1]);
            })
            .or_insert(HashSet::from([values[1]]));
    }

    let mut entries: Entries = vec![];
    for line in lines {
        let entry = line
            .split(',')
            .map(|v| v.parse::<u64>().unwrap())
            .collect::<Vec<_>>();

        entries.push(entry);
    }

    (order, entries)
}

pub fn part_one(order: &Order, entries: &Entries) -> u64 {
    let mut sum_middle_pages = 0;
    for entry in entries {
        if is_valid(order, entry) {
            // dbg!(valid, entry);
            let midway = entry.len() / 2;
            let midway = entry.get(midway).expect("middle page must exist");

            // dbg!(midway);
            sum_middle_pages += midway;
        }
    }

    sum_middle_pages
}

fn is_valid(order: &Order, entry: &Entry) -> bool {
    for (i, value) in entry.iter().enumerate() {
        let expected_succeeding = order.get(value);

        let remaining = &entry[(i + 1)..];
        let visited = &entry[..i];

        // dbg!(value, &expected_succeeding, remaining);

        if let Some(expected_succeeding) = expected_succeeding {
            if remaining
                .iter()
                .all(|r| expected_succeeding.contains(r))
                .not()
                || visited.iter().any(|v| expected_succeeding.contains(v))
            {
                // dbg!(valid);
                return false;
            }
        }
    }

    true
}

// TODO: move this to common crate.
macro_rules! measure_time {
    ($code:expr) => {{
        use std::time::Instant;
        let start = Instant::now();
        let result = $code;
        let elapsed = start.elapsed();
        println!("{} => {:?}", stringify!($code), elapsed);
        result
    }};
}

pub fn part_two(order: &Order, entries: &Entries) -> u64 {
    let mut sum_middle_pages = 0;
    for entry in entries {
        if measure_time!(is_valid(order, entry).not()) {
            let corrected_entry = measure_time!(find_corrected_entry(order, entry));

            if let Some(corrected_entry) = corrected_entry {
                let midway = corrected_entry.len() / 2;
                let midway = corrected_entry[midway];

                sum_middle_pages += midway;
            }
        }
    }

    sum_middle_pages
}

fn find_corrected_entry(order: &Order, entry: &Entry) -> Option<Entry> {
    let starting_node = find_starting_node(order, entry);
    let stopping_node = find_stopping_node(order, entry);

    find_path(order, entry, vec![], starting_node, stopping_node)
}

fn find_path(
    order: &Order,
    entry: &Entry,
    path: Entry,
    starting_node: u64,
    stopping_node: u64,
) -> Option<Entry> {
    if starting_node == stopping_node {
        // must cover all nodes
        if path.len() + 1 != entry.len() {
            return None;
        }

        let mut final_path = path.clone();
        final_path.push(stopping_node);

        let final_path = Some(final_path);

        return final_path;
    }

    let mut path = path.clone();
    path.push(starting_node);

    if let Some(succeeding) = order.get(&starting_node) {
        let reachable_nodes = entry.iter().filter(|e| succeeding.contains(*e));

        for reachable_node in reachable_nodes {
            let rest = find_path(order, entry, path.clone(), *reachable_node, stopping_node);

            if let Some(rest) = rest {
                return Some(rest);
            }
        }
    }

    None
}

fn find_starting_node(order: &Order, entry: &Entry) -> u64 {
    for value in entry {
        let mut has_parent = false;
        for other in entry {
            if value == other {
                continue;
            }

            if let Some(succeeding) = order.get(other) {
                if succeeding.contains(value) {
                    has_parent = true;
                    break;
                }
            }
        }

        if has_parent.not() {
            return *value;
        }
    }

    unreachable!("detected cycle in: {entry:?}");
}

fn find_stopping_node(order: &Order, entry: &Entry) -> u64 {
    for value in entry {
        let mut has_child = false;
        for other in entry {
            if value == other {
                continue;
            }

            if let Some(succeeding) = order.get(value) {
                if succeeding.contains(other) {
                    has_child = true;
                    break;
                }
            }
        }

        if has_child.not() {
            return *value;
        }
    }

    unreachable!("detected cycle in: {entry:?}");
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(r#"47|53
97|61
47|61

97,47,61
75,29
61,13"#,
            (
                HashMap::from([(97, HashSet::from([61])), (47, HashSet::from([53, 61]))]),
                vec![vec![97, 47, 61], vec![75, 29], vec![61, 13]]
            )
        )]
    fn test_parse(#[case] input: &str, #[case] expected: (Order, Entries)) {
        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"#,
        143
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: u64) {
        let input = parse(input);

        let actual = part_one(&input.0, &input.1);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"#,
        123
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: u64) {
        let input = parse(input);

        let actual = part_two(&input.0, &input.1);

        assert_eq!(actual, expected);
    }
}
