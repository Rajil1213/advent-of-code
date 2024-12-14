use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RobotInfo {
    pub initial_position_x: isize,
    pub initial_position_y: isize,

    pub velocity_x: isize,
    pub velocity_y: isize,
}

impl RobotInfo {
    pub fn final_position(&self, time: usize) -> (isize, isize) {
        let time = time as isize;
        (
            self.initial_position_x + self.velocity_x * time,
            self.initial_position_y + self.velocity_y * time,
        )
    }
}

impl From<(isize, isize, isize, isize)> for RobotInfo {
    fn from(value: (isize, isize, isize, isize)) -> Self {
        Self {
            initial_position_x: value.0,
            initial_position_y: value.1,
            velocity_x: value.2,
            velocity_y: value.3,
        }
    }
}

pub fn parse(input: &str) -> Vec<RobotInfo> {
    input.lines().fold(Vec::new(), |mut info, line| {
        let mut location_and_velocity = line
            .split_whitespace()
            .map(|v| v.split("=").nth(1).unwrap())
            .take(2);

        let location = location_and_velocity.next().unwrap();
        let location = location
            .split(",")
            .map(|num| num.parse::<isize>().unwrap())
            .collect::<Vec<isize>>();

        let velocity = location_and_velocity.next().unwrap();
        let velocity = velocity
            .split(",")
            .map(|num| num.parse::<isize>().unwrap())
            .collect::<Vec<isize>>();

        info.push(RobotInfo::from((
            location[0],
            location[1],
            velocity[0],
            velocity[1],
        )));

        info
    })
}

pub fn part_one(
    info: &[RobotInfo],
    max_position_x: usize,
    max_position_y: usize,
    time: usize,
) -> usize {
    let (mid_x, mid_y) = (max_position_x / 2, max_position_y / 2);

    info.iter()
        .map(|info| final_position(info, time, max_position_x, max_position_y))
        .fold(HashMap::new(), |mut positions, (position_x, position_y)| {
            positions
                .entry((position_x, position_y))
                .and_modify(|v| *v += 1)
                .or_insert(1);

            positions
        })
        .iter()
        .fold([0, 0, 0, 0], |mut q, ((x, y), count)| {
            if *x < mid_x && *y < mid_y {
                q[0] += count;
            } else if *x < mid_x && *y > mid_y {
                q[1] += count;
            } else if *x > mid_x && *y < mid_y {
                q[2] += count;
            } else if *x > mid_x && *y > mid_y {
                q[3] += count;
            }

            q
        })
        .iter()
        .product()
}

pub fn part_two(info: &[RobotInfo], max_position_x: usize, max_position_y: usize) -> usize {
    let mut time = 0;
    loop {
        let positions = info
            .iter()
            .map(|info| final_position(info, time, max_position_x, max_position_y))
            .collect::<HashSet<(usize, usize)>>();

        if is_christmas_tree(&positions, max_position_x, max_position_y) {
            return time;
        }

        time += 1;

        if time % 1000000 == 0 {
            println!("Time elapsed = {time} seconds");
        }
    }
}

fn final_position(
    info: &RobotInfo,
    time: usize,
    max_position_x: usize,
    max_position_y: usize,
) -> (usize, usize) {
    let (final_position_x, final_position_y) = info.final_position(time);

    let final_position_x = if final_position_x >= 0 {
        (final_position_x as usize) % max_position_x
    } else {
        (max_position_x - ((-final_position_x as usize) % max_position_x)) % max_position_x
    };

    let final_position_y = if final_position_y >= 0 {
        (final_position_y as usize) % max_position_y
    } else {
        (max_position_y - ((-final_position_y as usize) % max_position_y)) % max_position_y
    };

    (final_position_x, final_position_y)
}

fn is_christmas_tree(
    positions: &HashSet<(usize, usize)>,
    max_position_x: usize,
    max_position_y: usize,
) -> bool {
    for row in 0..max_position_y {
        let mut count = 0;
        for col in 0..max_position_x {
            let continuous = positions.contains(&(col, row));
            if continuous {
                count += 1;

                if count > 10 {
                    show_positions(positions, max_position_x, max_position_y);

                    return true;
                }
            } else {
                count = 0;
            }
        }
    }

    false
}

pub fn show_positions(
    positions: &HashSet<(usize, usize)>,
    max_position_x: usize,
    max_position_y: usize,
) {
    for row in 0..max_position_y {
        for col in 0..max_position_x {
            if positions.contains(&(col, row)) {
                print!("#");
            } else {
                print!(".")
            }
        }

        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(r#"p=0,4 v=3,-3
p=6,3 v=-1,-3"#, vec![
            RobotInfo { initial_position_x: 0, initial_position_y: 4, velocity_x: 3, velocity_y: -3 },
            RobotInfo { initial_position_x: 6, initial_position_y: 3, velocity_x: -1, velocity_y: -3 }
        ])]
    fn test_parse(#[case] input: &str, #[case] expected: Vec<RobotInfo>) {
        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case((r#"p=2,4 v=2,-3"#, 5, 11, 7), (1, 3))]
    #[case((r#"p=2,4 v=2,-3"#, 5, 11, 8), (1, 5))]
    fn test_filter_final_position(
        #[case] (input, time, max_position_x, max_position_y): (&str, usize, usize, usize),
        #[case] expected: (usize, usize),
    ) {
        let info = parse(input)[0];

        let actual = final_position(&info, time, max_position_x, max_position_y);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"#,
        12
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let info = parse(input);

        let actual = part_one(&info, 11, 7, 100);

        assert_eq!(actual, expected);
    }
}
