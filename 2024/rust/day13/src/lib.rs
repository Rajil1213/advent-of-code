#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Equation {
    pub a1: isize,
    pub a2: isize,

    pub b1: isize,
    pub b2: isize,

    pub target_x: isize,
    pub target_y: isize,
}

pub fn parse(input: &str) -> Vec<Equation> {
    let lines = input.lines().collect::<Vec<&str>>();

    lines
        .iter()
        .enumerate()
        .step_by(4)
        .map(|(i, _)| {
            let mut coeffs1 = lines[i]
                .split(":")
                .nth(1)
                .unwrap()
                .split(",")
                .map(|val| val.split("+").nth(1).unwrap())
                .take(2);
            let mut coeffs2 = lines[i + 1]
                .split(":")
                .nth(1)
                .unwrap()
                .split(",")
                .map(|val| val.split("+").nth(1).unwrap())
                .take(2);
            let mut targets = lines[i + 2]
                .split(":")
                .nth(1)
                .unwrap()
                .split(",")
                .map(|val| val.split("=").nth(1).unwrap())
                .take(2);

            Equation {
                a1: coeffs1.next().unwrap().parse().unwrap(),
                a2: coeffs1.next().unwrap().parse().unwrap(),

                b1: coeffs2.next().unwrap().parse().unwrap(),
                b2: coeffs2.next().unwrap().parse().unwrap(),

                target_x: targets.next().unwrap().parse().unwrap(),
                target_y: targets.next().unwrap().parse().unwrap(),
            }
        })
        .collect()
}

pub fn part_one(equations: &[Equation]) -> isize {
    equations
        .iter()
        .filter_map(|eq| {
            let Equation {
                a1,
                a2,
                b1,
                b2,
                target_x,
                target_y,
            } = eq;

            let det = a1 * b2 - b1 * a2;

            if det == 0 {
                return None;
            }

            // Apply Cramer's rule
            let num_buttons_a = (b2 * target_x - b1 * target_y) / det;
            let num_buttons_b = (a1 * target_y - a2 * target_x) / det;

            if (0..=100).contains(&num_buttons_a)
                && (0..=100).contains(&num_buttons_b)
                && (a1 * num_buttons_a + b1 * num_buttons_b) == *target_x
                && (a2 * num_buttons_a + b2 * num_buttons_b) == *target_y
            {
                return Some(num_buttons_a * 3 + num_buttons_b);
            }

            None
        })
        .sum()
}

pub fn part_two(equations: &[Equation]) -> isize {
    const OFFSET: isize = 10000000000000;

    equations
        .iter()
        .filter_map(|eq| {
            let Equation {
                a1,
                a2,
                b1,
                b2,
                mut target_x,
                mut target_y,
            } = eq;

            target_x += OFFSET;
            target_y += OFFSET;

            let det = a1 * b2 - b1 * a2;

            if det == 0 {
                return None;
            }

            // Apply Cramer's rule
            let num_buttons_a = (b2 * target_x - b1 * target_y) / det;
            let num_buttons_b = (a1 * target_y - a2 * target_x) / det;

            if (a1 * num_buttons_a + b1 * num_buttons_b) == target_x
                && (a2 * num_buttons_a + b2 * num_buttons_b) == target_y
            {
                return Some(num_buttons_a * 3 + num_buttons_b);
            }

            None
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(r#"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176"#, vec![
            Equation {a1: 94, a2: 34, b1: 22, b2: 67, target_x: 8400, target_y: 5400 },
            Equation {a1: 26, a2: 66, b1: 67, b2: 21, target_x: 12748, target_y: 12176 },
    ])]
    fn test_parse(#[case] input: &str, #[case] expected: Vec<Equation>) {
        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"#,
        480
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: isize) {
        let equations = parse(input);

        let actual = part_one(&equations);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"#,
        480
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: isize) {
        let equations = parse(input);

        let actual = part_two(&equations);

        assert_eq!(actual, expected);
    }
}
