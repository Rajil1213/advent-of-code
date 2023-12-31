use svg::{node::element::Circle, Document};

const INITIAL_POINT: usize = 10000000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Point {
    pub position: (usize, usize),
    pub color: String,
}

impl Point {
    pub fn from(position: (usize, usize), color: &str) -> Self {
        Self {
            position,
            color: color.to_string(),
        }
    }
}

#[derive(Debug)]
pub enum Direction {
    R,
    L,
    D,
    U,
}

impl Direction {
    pub fn from_char(ch: &char) -> Self {
        match ch {
            'R' | '0' => Self::R,
            'L' | '2' => Self::L,
            'U' | '3' => Self::U,
            'D' | '1' => Self::D,
            _ => panic!("Invalid direction: {ch} found"),
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            Direction::R => 'R',
            Direction::L => 'L',
            Direction::D => 'D',
            Direction::U => 'U',
        }
    }
}

fn next_point(
    current_point: (usize, usize),
    distance: usize,
    direction: &Direction,
) -> (usize, usize) {
    let (x, y) = current_point;
    match direction {
        Direction::R => (x, y + distance),
        Direction::L => (x, y.saturating_sub(distance)),
        Direction::U => (x.saturating_sub(distance), y),
        Direction::D => (x + distance, y),
    }
}

pub fn parse(input: &str) -> Vec<Point> {
    let mut dig_plan: Vec<Point> = vec![];

    // start somewhere in the middle
    let mut x: usize = INITIAL_POINT;
    let mut y: usize = INITIAL_POINT;

    for line in input.lines() {
        let values: [&str; 3] = line
            .split(' ')
            .collect::<Vec<&str>>()
            .try_into()
            .expect("each line must contain three space-separated strings");

        let direction = Direction::from_char(&values[0].chars().next().unwrap());
        let distance = values[1]
            .parse::<usize>()
            .expect("second value in the line must be a valid number");
        let color = values[2]
            .strip_prefix('(')
            .expect("third value in line must begin with `(`")
            .strip_suffix(')')
            .expect("third value in the line must end with `)`");

        // for _ in 0..distance {
        (x, y) = next_point((x, y), distance, &direction);
        let new_point = Point::from((x, y), color);
        if dig_plan.contains(&new_point) {
            continue;
        }
        dig_plan.push(new_point);
        // }
    }

    dig_plan
}

pub fn calculate_lava_volume(dig_plan: &[Point]) -> usize {
    let path_len = dig_plan.len();

    let mut perimeter: usize = dig_plan[0].position.0.abs_diff(INITIAL_POINT)
        + dig_plan[0].position.1.abs_diff(INITIAL_POINT);

    // Shoelace formula
    let mut double_area: isize = 0;
    for i in 0..path_len - 1 {
        let x_diff = dig_plan[i].position.0 as isize - dig_plan[i + 1].position.0 as isize;
        let y_diff = dig_plan[i].position.1 as isize - dig_plan[i + 1].position.1 as isize;
        // dbg!(x_diff, y_diff);
        let y_sum = (dig_plan[i].position.1 + dig_plan[i + 1].position.1) as isize;

        double_area += x_diff * y_sum;
        // one of x_diff and y_diff will always be zero
        perimeter += (x_diff + y_diff).unsigned_abs();
    }

    let double_area = double_area.unsigned_abs();
    let inner_holes = (double_area + 2 - perimeter) / 2; // Pick's theorem

    inner_holes + perimeter
}

pub fn create_matrix(dig_plan: &[Point]) -> Vec<Vec<String>> {
    let mut max_x = 0;
    let mut max_y = 0;

    for point in dig_plan {
        if point.position.0 >= max_x {
            max_x = point.position.0 + 1;
        }

        if point.position.1 >= max_y {
            max_y = point.position.1 + 1;
        }
    }

    // dbg!(max_x, max_y);
    let mut matrix: Vec<Vec<String>> = vec![vec!["".to_string(); max_y]; max_x];

    for point in dig_plan {
        matrix[point.position.0][point.position.1] = point.color.clone();
    }

    matrix
}

pub fn convert_matrix_to_svg(matrix: &[Vec<String>], path: &str) {
    let mut document = Document::new()
        .set("width", "9000")
        .set("height", "9000")
        .set("fill", "#ffffff");

    for (i, row) in matrix.iter().enumerate() {
        for (j, color) in row.iter().enumerate() {
            document = document.add(
                Circle::new()
                    .set("cx", 10 * j + 10)
                    .set("cy", 10 * i + 10)
                    .set("r", 4)
                    .set("fill", color.clone()),
            );
        }
    }

    svg::save(path, &document).unwrap();
}

pub fn create_actual_input(corrupted_input: &[Point]) -> String {
    let mut actual_input = String::new();

    for point in corrupted_input {
        let color = point.color.clone();
        let direction = Direction::from_char(
            &color
                .chars()
                .last()
                .expect("Color must have at least one char"),
        );
        let distance = color.chars().take(color.len() - 1).collect::<String>();
        let distance = distance.strip_prefix('#').expect("Color must begin with #");
        let distance = usize::from_str_radix(distance, 16)
            .expect("First four chars of color except # must be a valid hex");

        actual_input.push_str(&format!("{} {distance} ({color})\n", direction.to_char()));
    }

    actual_input.strip_suffix('\n').unwrap().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calculates_area_correctly() {
        let input: &str = "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)";

        let dig_plan = parse(input);

        // let matrix = create_matrix(&dig_plan);
        // convert_matrix_to_svg(&matrix, "test_path.svg");

        assert_eq!(calculate_lava_volume(&dig_plan), 62);
    }

    #[test]
    fn calculates_actual_area_correctly() {
        let corrupted_input: &str = "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)";

        let dig_plan = parse(corrupted_input);
        let corrected_input = create_actual_input(&dig_plan);
        let actual_dig_plan = parse(&corrected_input);

        assert_eq!(calculate_lava_volume(&actual_dig_plan), 952408144115);
    }
}
