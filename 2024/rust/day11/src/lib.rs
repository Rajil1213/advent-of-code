use std::collections::HashMap;

pub fn parse(input: &str) -> Vec<usize> {
    input
        .lines()
        .next()
        .unwrap()
        .split_whitespace()
        .map(|d| d.parse::<usize>().unwrap())
        .collect()
}

pub fn part_one(stones: &[usize], num_blinks: usize) -> usize {
    let mut num_stones = 0;

    let mut memo: HashMap<(usize, usize), usize> = HashMap::new();
    for stone in stones.iter() {
        let count = calculate_stones(*stone, num_blinks, &mut memo);

        num_stones += count;
    }

    num_stones
}

fn calculate_stones(
    stone: usize,
    num_blinks: usize,
    memo: &mut HashMap<(usize, usize), usize>,
) -> usize {
    if let Some(count) = memo.get(&(stone, num_blinks)) {
        return *count;
    }

    if num_blinks == 0 {
        memo.insert((stone, num_blinks), 1);
        return 1;
    }

    if stone == 0 {
        let count = calculate_stones(1, num_blinks - 1, memo);
        memo.insert((stone, num_blinks), count);

        return count;
    }

    let num_digits = stone.to_string().len();

    if num_digits % 2 == 0 {
        let divisor = 10u32.pow((num_digits / 2) as u32) as usize;

        let first_stone = stone / divisor;
        let first_count = calculate_stones(first_stone, num_blinks - 1, memo);

        let second_stone = stone % divisor;
        let second_count = calculate_stones(second_stone, num_blinks - 1, memo);

        let total_count = first_count + second_count;
        memo.insert((stone, num_blinks), total_count);

        return total_count;
    }

    let count = calculate_stones(stone * 2024, num_blinks - 1, memo);
    memo.insert((stone, num_blinks), count);

    count
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(("0 1 10 99 999", 1), 7)]
    #[case(("125 17", 6), 22)]
    #[case(("125 17", 25), 55312)]
    fn test_part_one(#[case] (input, num_blinks): (&str, usize), #[case] expected: usize) {
        let stones = parse(input);

        let actual = part_one(&stones, num_blinks);

        assert_eq!(actual, expected);
    }
}
