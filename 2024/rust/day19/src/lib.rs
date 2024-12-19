use std::collections::{BTreeSet, HashMap};

pub type Available = BTreeSet<String>;
pub type TargetDesigns = Vec<String>;

pub fn parse(input: &str) -> (Available, TargetDesigns) {
    let mut lines = input.lines();

    let available_towels = lines
        .next()
        .unwrap()
        .split(", ")
        .map(|towel| towel.to_string())
        .collect::<BTreeSet<String>>();

    lines.next();

    let target_designs = lines.map(|s| s.to_string()).collect();

    (available_towels, target_designs)
}

pub fn part_one(available_towels: &Available, target_designs: &TargetDesigns) -> usize {
    let mut memo = HashMap::new();

    target_designs
        .iter()
        .filter(|target| get_count(available_towels, target, &mut memo) > 0)
        .count()
}

pub fn part_two(available_towels: &Available, target_designs: &TargetDesigns) -> usize {
    let mut memo = HashMap::new();

    target_designs
        .iter()
        .map(|target| get_count(available_towels, target, &mut memo))
        .sum()
}

fn get_count(
    available_towels: &Available,
    target_design: &str,
    memo: &mut HashMap<String, usize>,
) -> usize {
    if let Some(&result) = memo.get(target_design) {
        return result;
    }

    if target_design.is_empty() {
        return 1;
    }

    let total_ways = available_towels.iter().fold(0, |total_ways, towel| {
        if target_design.starts_with(towel) {
            total_ways + get_count(available_towels, &target_design[towel.len()..], memo)
        } else {
            total_ways
        }
    });

    memo.insert(target_design.to_string(), total_ways);

    total_ways
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        r#"r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"#,
        6
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let (available_towels, target_designs) = parse(input);

        let actual = part_one(&available_towels, &target_designs);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"#,
        16
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let (available_towels, target_designs) = parse(input);

        let actual = part_two(&available_towels, &target_designs);

        assert_eq!(actual, expected);
    }
}
