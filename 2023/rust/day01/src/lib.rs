pub fn get_calibration_value(input: &str) -> u64 {
    let nums = input
        .chars()
        .filter(|c| c.is_ascii_digit())
        .collect::<Vec<char>>();

    let nums_len = nums.len();

    format!("{}{}", nums[0], nums[nums_len - 1])
        .parse::<u64>()
        .expect("always a valid number")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gets_correct_calibration_val() {
        struct TestCase<'a> {
            input: &'a str,
            expected_output: u64,
        }

        let test_cases: Vec<TestCase> = vec![
            TestCase {
                input: "1abc2",
                expected_output: 12,
            },
            TestCase {
                input: "pqr3stu8vwx",
                expected_output: 38,
            },
            TestCase {
                input: "a1b2c3d4e5f",
                expected_output: 15,
            },
            TestCase {
                input: "treb7uchet",
                expected_output: 77,
            },
        ];

        for TestCase {
            input,
            expected_output,
        } in test_cases
        {
            let result = get_calibration_value(input);
            assert_eq!(result, expected_output);
        }
    }
}
