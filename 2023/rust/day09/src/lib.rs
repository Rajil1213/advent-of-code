pub fn get_next_value(list: &Vec<i64>) -> i64 {
    let length = list.len();
    let mut diff_nums: Vec<i64> = Vec::with_capacity(length - 1);

    assert!(
        length.gt(&1),
        "fatal error: reached end without getting constant diff"
    );

    for i in 1..length {
        diff_nums.push(list[i] - list[i - 1]);
    }

    let current_last = list[length - 1];
    if diff_nums.iter().all(|v| v.eq(&0)) {
        current_last + diff_nums[diff_nums.len() - 1]
    } else {
        current_last + get_next_value(&diff_nums)
    }
}

pub fn get_previous_value(list: &Vec<i64>) -> i64 {
    let length = list.len();
    let mut diff_nums: Vec<i64> = Vec::with_capacity(length - 1);

    assert!(
        length.gt(&1),
        "fatal error: reached end without getting constant diff"
    );

    for i in 1..length {
        diff_nums.push(list[i] - list[i - 1]);
    }

    let current_first = list[0];
    if diff_nums.iter().all(|v| v.eq(&0)) {
        current_first - diff_nums[diff_nums.len() - 1]
    } else {
        current_first - get_previous_value(&diff_nums)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(vec![0, 3, 6, 9, 12, 15], 18)]
    #[case(vec![1, 3, 6, 10, 15, 21], 28)]
    #[case(vec![10, 13, 16, 21, 30, 45], 68)]
    #[case(vec![4, 0, -4, -8, -12, -16, -20, -24, -28, -32, -36, -40, -44, -48, -52, -56, -60, -64, -68, -72, -76], -80)]
    fn calculates_next_value_correctly(#[case] input: Vec<i64>, #[case] expected: i64) {
        assert_eq!(get_next_value(&input), expected);
    }

    #[rstest]
    #[case(vec![0, 3, 6, 9, 12, 15], -3)]
    #[case(vec![1, 3, 6, 10, 15, 21], 0)]
    #[case(vec![10, 13, 16, 21, 30, 45], 5)]
    #[case(vec![4, 0, -4, -8, -12, -16, -20, -24, -28, -32, -36, -40, -44, -48, -52, -56, -60, -64, -68, -72, -76], 8)]
    fn calculates_previous_value_correctly(#[case] input: Vec<i64>, #[case] expected: i64) {
        assert_eq!(get_previous_value(&input), expected);
    }
}
