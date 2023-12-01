use std::collections::HashMap;

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

pub fn get_real_calibration_value(input: &str) -> u64 {
    let spelled_num_map: HashMap<&str, char> = HashMap::from([
        ("one", '1'),
        ("two", '2'),
        ("three", '3'),
        ("four", '4'),
        ("five", '5'),
        ("six", '6'),
        ("seven", '7'),
        ("eight", '8'),
        ("nine", '9'),
    ]);

    let mut nums: Vec<char> = vec![];
    let input_len = input.len();

    let mut skip = 0;
    while skip < input_len {
        let rest = input.chars().skip(skip).collect::<String>();

        let first_char = rest
            .chars()
            .next()
            .expect("should contain at least one char");

        if first_char.is_ascii_digit() {
            nums.push(first_char);
        }
        // check spelled out numerals
        // needs at least three chars viz., one, two, six)
        else if rest.len() > 2 {
            for spelled_numeral in spelled_num_map.keys() {
                if rest.starts_with(spelled_numeral) {
                    nums.push(spelled_num_map[spelled_numeral]);
                    break;
                }
            }
        }

        skip += 1;
    }

    format!("{}{}", nums[0], nums[nums.len() - 1])
        .parse::<u64>()
        .expect("should always be an int")
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestCase<'a> {
        input: &'a str,
        expected: u64,
    }

    #[test]
    fn gets_correct_calibration_val() {
        let test_cases: Vec<TestCase> = vec![
            TestCase {
                input: "1abc2",
                expected: 12,
            },
            TestCase {
                input: "pqr3stu8vwx",
                expected: 38,
            },
            TestCase {
                input: "a1b2c3d4e5f",
                expected: 15,
            },
            TestCase {
                input: "treb7uchet",
                expected: 77,
            },
        ];

        for TestCase { input, expected } in test_cases {
            let actual = get_calibration_value(input);
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn gets_correct_real_calibration_val() {
        let test_cases: Vec<TestCase> = vec![
            TestCase {
                input: "two1nine",
                expected: 29,
            },
            TestCase {
                input: "eightwothree",
                expected: 83,
            },
            TestCase {
                input: "abcone2threexyz",
                expected: 13,
            },
            TestCase {
                input: "xtwone3four",
                expected: 24,
            },
            TestCase {
                input: "4nineeightseven2",
                expected: 42,
            },
            TestCase {
                input: "zoneight234",
                expected: 14,
            },
            TestCase {
                input: "7pqrstsixteen",
                expected: 76,
            },
            // personal test case; overlapping numerals
            TestCase {
                input: "threetwone",
                expected: 31,
            },
        ];

        for TestCase { input, expected } in test_cases {
            let actual = get_real_calibration_value(input);
            assert_eq!(actual, expected, "{}", input);
        }
    }
}
