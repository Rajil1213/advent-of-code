#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl From<char> for Direction {
    fn from(value: char) -> Self {
        match value {
            '^' => Direction::Up,
            'v' => Direction::Down,
            '<' => Direction::Left,
            '>' => Direction::Right,
            _ => unreachable!("invalid position char"),
        }
    }
}

impl Direction {
    pub fn delta(&self) -> (isize, isize) {
        match self {
            Direction::Left => (0, -1),
            Direction::Right => (0, 1),
            Direction::Up => (-1, 0),
            Direction::Down => (1, 0),
        }
    }
}

pub type Map = Vec<Vec<char>>;
pub type InitialLocation = (usize, usize);
pub type Directions = Vec<Direction>;

pub const INITIAL: char = '@';
pub const BOX: char = 'O';
pub const WALL: char = '#';
pub const EMPTY: char = '.';

pub fn parse(input: &str) -> (Map, InitialLocation, Directions) {
    let mut map: Map = vec![];
    let mut initial_location: InitialLocation = (0, 0);
    let mut lines = input.lines().enumerate();

    loop {
        let (row_num, line) = lines.next().unwrap();

        if line.is_empty() {
            break;
        }

        let mut row = vec![];
        for (col_num, c) in line.chars().enumerate() {
            if c == INITIAL {
                initial_location = (row_num, col_num);
            }

            row.push(c);
        }

        map.push(row);
    }

    let mut directions: Directions = vec![];
    for (_, line) in lines {
        for c in line.chars() {
            directions.push(Direction::from(c));
        }
    }

    (map, initial_location, directions)
}

pub fn part_one(map: &mut Map, initial_location: InitialLocation, directions: Directions) -> usize {
    let (mut location_x, mut location_y) = initial_location;

    for direction in directions {
        let (delta_x, delta_y) = direction.delta();

        let (new_location_x, new_location_y) =
            (location_x as isize + delta_x, location_y as isize + delta_y);

        let new_location_x = new_location_x as usize;
        let new_location_y = new_location_y as usize;

        match map[new_location_x][new_location_y] {
            BOX => {
                (location_x, location_y) =
                    push_boxes(map, (location_x, location_y), (delta_x, delta_y));
            }
            EMPTY => {
                map[location_x][location_y] = EMPTY;
                map[new_location_x][new_location_y] = INITIAL;

                (location_x, location_y) = (new_location_x, new_location_y);
            }
            WALL => {}
            _ => unreachable!(
                "invalid character at ({}, {}): {}",
                new_location_x, new_location_y, map[new_location_x][new_location_y]
            ),
        }

        // println!("direction: {direction:?}");
        // show_map(map);
    }

    show_map(map);

    calculate_gps(map)
}

pub fn push_boxes(
    map: &mut Map,
    location: (usize, usize),
    delta: (isize, isize),
) -> (usize, usize) {
    let (mut location_x, mut location_y) = location;
    let (delta_x, delta_y) = delta;

    loop {
        let (new_location_x, new_location_y) =
            (location_x as isize + delta_x, location_y as isize + delta_y);

        let new_location_x = new_location_x as usize;
        let new_location_y = new_location_y as usize;

        match map[new_location_x][new_location_y] {
            EMPTY => {
                map[new_location_x][new_location_y] = BOX;
                map[location.0][location.1] = EMPTY;

                let new_initial_x = (location.0 as isize + delta_x) as usize;
                let new_initial_y = (location.1 as isize + delta_y) as usize;

                map[new_initial_x][new_initial_y] = INITIAL;

                return (new_initial_x, new_initial_y);
            }
            BOX => {
                location_x = new_location_x;
                location_y = new_location_y;
            }
            WALL => {
                return (location.0, location.1);
            }
            _ => unreachable!(
                "invalid character at ({}, {}): {}",
                new_location_x, new_location_y, map[new_location_x][new_location_y]
            ),
        }
    }
}

fn show_map(map: &Map) {
    for line in map {
        println!("{}", line.iter().collect::<String>());
    }
}

fn calculate_gps(map: &Map) -> usize {
    let mut sum = 0;

    for (row_num, row) in map.iter().enumerate() {
        for (col_num, c) in row.iter().enumerate() {
            match *c {
                BOX => {
                    // dbg!(col_num, row_num);
                    sum += 100 * row_num + col_num;
                }
                LEFT_EDGE => {
                    sum += 100 * row_num + col_num;
                }
                _ => {
                    continue;
                }
            }
        }
    }

    sum
}

pub const LEFT_EDGE: char = '[';
pub const RIGHT_EDGE: char = ']';

pub fn part_two(map: &Map, directions: Directions) -> usize {
    let (mut map, initial_location) = expand_map(map);
    let (mut location_x, mut location_y) = initial_location;

    for direction in directions {
        let (delta_x, delta_y) = direction.delta();

        let (new_location_x, new_location_y) =
            (location_x as isize + delta_x, location_y as isize + delta_y);

        let new_location_x = new_location_x as usize;
        let new_location_y = new_location_y as usize;

        // println!("direction: {direction:?}");
        match map[new_location_x][new_location_y] {
            LEFT_EDGE | RIGHT_EDGE => {
                (location_x, location_y) =
                    push_expanded_boxes(&mut map, (location_x, location_y), direction);
            }
            EMPTY => {
                map[location_x][location_y] = EMPTY;
                map[new_location_x][new_location_y] = INITIAL;

                (location_x, location_y) = (new_location_x, new_location_y);
            }
            WALL => {}
            _ => unreachable!(
                "invalid character at ({}, {}): {}",
                new_location_x, new_location_y, map[new_location_x][new_location_y]
            ),
        }

        // show_map(&map);
    }

    show_map(&map);

    calculate_gps(&map)
}

fn expand_map(map: &Map) -> (Map, InitialLocation) {
    show_map(map);
    let mut expanded_map = vec![];

    for line in map {
        let mut expanded_map_row = vec![];
        for c in line {
            match *c {
                WALL => {
                    expanded_map_row.push(WALL);
                    expanded_map_row.push(WALL);
                }

                BOX => {
                    expanded_map_row.push(LEFT_EDGE);
                    expanded_map_row.push(RIGHT_EDGE);
                }

                INITIAL => {
                    expanded_map_row.push(INITIAL);
                    expanded_map_row.push(EMPTY);
                }

                EMPTY => {
                    expanded_map_row.push(EMPTY);
                    expanded_map_row.push(EMPTY);
                }

                invalid => unreachable!("invalid character at {:?}", invalid),
            }
        }

        expanded_map.push(expanded_map_row);
    }

    for (i, line) in expanded_map.iter().enumerate() {
        let initial_position_y =
            line.iter()
                .enumerate()
                .find_map(|(j, c)| if *c == INITIAL { Some(j) } else { None });

        if let Some(initial_position_y) = initial_position_y {
            return (expanded_map, (i, initial_position_y));
        }
    }

    unreachable!("initial position must exist");
}

pub fn push_expanded_boxes(
    map: &mut Map,
    location: (usize, usize),
    direction: Direction,
) -> (usize, usize) {
    let (mut location_x, mut location_y) = location;
    let (delta_x, delta_y) = direction.delta();

    // dbg!(next_empty);

    match direction {
        Direction::Up | Direction::Down => {
            let pushable = is_vertically_pushable(map, location, direction);

            if pushable {
                push_vertical(map, location, direction);

                return ((location.0 as isize + delta_x) as usize, location.1);
            }

            (location.0, location.1)
        }
        Direction::Left | Direction::Right => {
            let next_empty = get_next_empty(map, location, (delta_x, delta_y));
            if let Some((next_empty_x, next_empty_y)) = next_empty {
                loop {
                    let (next_location_x, next_location_y) =
                        (location_x as isize + delta_x, location_y as isize + delta_y);

                    let next_location_x = next_location_x as usize;
                    let next_location_y = next_location_y as usize;

                    if (next_location_x, next_location_y) == (next_empty_x, next_empty_y) {
                        map[location.0][location.1] = EMPTY;

                        match direction {
                            Direction::Left => map[next_location_x][next_location_y] = LEFT_EDGE,
                            Direction::Right => map[next_location_x][next_location_y] = RIGHT_EDGE,
                            _ => unreachable!("this match block should only handle left/right"),
                        }

                        let (new_initial_x, new_initial_y) = (
                            (location.0 as isize + delta_x) as usize,
                            (location.1 as isize + delta_y) as usize,
                        );

                        map[new_initial_x][new_initial_y] = INITIAL;

                        return (new_initial_x, new_initial_y);
                    }

                    if map[next_location_x][next_location_y] == LEFT_EDGE {
                        map[next_location_x][next_location_y] = RIGHT_EDGE;
                    } else {
                        // next_empty would be `None` if it was neither `LEFT_EDGE` nor `RIGHT_EDGE`
                        map[next_location_x][next_location_y] = LEFT_EDGE;
                    }

                    location_x = next_location_x;
                    location_y = next_location_y;
                }
            }

            (location.0, location.1)
        }
    }
}

fn get_next_empty(
    map: &Map,
    location: InitialLocation,
    delta: (isize, isize),
) -> Option<(usize, usize)> {
    let (mut location_x, mut location_y) = location;
    let (delta_x, delta_y) = delta;

    loop {
        let (new_location_x, new_location_y) =
            (location_x as isize + delta_x, location_y as isize + delta_y);

        let (new_location_x, new_location_y) = (new_location_x as usize, new_location_y as usize);

        match map[new_location_x][new_location_y] {
            EMPTY => {
                return Some((new_location_x, new_location_y));
            }

            WALL => {
                return None;
            }

            _ => {
                location_x = new_location_x;
                location_y = new_location_y;
                continue;
            }
        }
    }
}

fn is_vertically_pushable(map: &Map, location: InitialLocation, direction: Direction) -> bool {
    let (location_x, location_y) = location;

    match direction {
        Direction::Up | Direction::Down => {
            let (delta_x, _delta_y) = direction.delta();
            let (next_location_x, next_location_y) = (location_x as isize + delta_x, location_y);

            let next_location_x = next_location_x as usize;

            match map[next_location_x][next_location_y] {
                LEFT_EDGE => {
                    let left =
                        is_vertically_pushable(map, (next_location_x, next_location_y), direction);
                    let right = is_vertically_pushable(
                        map,
                        (next_location_x, next_location_y + 1),
                        direction,
                    );

                    left && right
                }

                RIGHT_EDGE => {
                    let left = is_vertically_pushable(
                        map,
                        (next_location_x, next_location_y - 1),
                        direction,
                    );
                    let right =
                        is_vertically_pushable(map, (next_location_x, next_location_y), direction);

                    left && right
                }
                EMPTY => true,
                WALL => false,
                _ => unreachable!(
                    "invalid char at ({}, {}): {}",
                    next_location_x, next_location_y, map[next_location_x][next_location_y]
                ),
            }
        }
        Direction::Left | Direction::Right => unreachable!("only call this with up/down"),
    }
}

fn push_vertical(map: &mut Map, location: (usize, usize), direction: Direction) {
    let (location_x, location_y) = location;
    match direction {
        Direction::Up | Direction::Down => {
            let (delta_x, _delta_y) = direction.delta();

            let (next_location_x, next_location_y) = (location_x as isize + delta_x, location_y);
            let next_location_x = next_location_x as usize;

            match map[next_location_x][next_location_y] {
                LEFT_EDGE => {
                    // left
                    push_vertical(map, (next_location_x, next_location_y), direction);
                    // right
                    push_vertical(map, (next_location_x, next_location_y + 1), direction);

                    map[next_location_x][next_location_y] = map[location.0][location.1];
                    map[location.0][location.1] = EMPTY;
                }

                RIGHT_EDGE => {
                    push_vertical(map, (next_location_x, next_location_y - 1), direction);
                    push_vertical(map, (next_location_x, next_location_y), direction);

                    map[next_location_x][next_location_y] = map[location.0][location.1];
                    map[location.0][location.1] = EMPTY;
                }
                EMPTY => {
                    map[next_location_x][next_location_y] = map[location.0][location.1];
                    map[location.0][location.1] = EMPTY;
                }
                _ => unreachable!(
                    "invalid char at ({}, {}): {}",
                    next_location_x, next_location_y, map[next_location_x][next_location_y]
                ),
            }
        }
        _ => unreachable!("must be up or down"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(r#"########
#..O.O.#
##@.O..#
########

<^v>"#, (
            vec![
                    vec!['#', '#', '#', '#', '#', '#', '#', '#'],
                    vec!['#', '.', '.', 'O', '.', 'O', '.', '#'],
                    vec!['#', '#', '@', '.', 'O', '.', '.', '#'],
                    vec!['#', '#', '#', '#', '#', '#', '#', '#']
                ],
            (2, 2),
            vec![Direction::Left, Direction::Up, Direction::Down, Direction::Right]
        )
    )]
    fn test_parse(#[case] input: &str, #[case] expected: (Map, InitialLocation, Directions)) {
        let (map, initial_location, directions) = parse(input);

        assert_eq!(map, expected.0);
        assert_eq!(initial_location, expected.1);
        assert_eq!(directions, expected.2);
    }

    #[rstest]
    #[case(
        r#"########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"#,
        2028
    )]
    #[case(
        r#"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"#,
        10092
    )]
    fn test_part_one(#[case] input: &str, #[case] expected: usize) {
        let (mut map, initial_location, directions) = parse(input);

        let actual = part_one(&mut map, initial_location, directions);

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case(
        r#"#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^>"#,
        618
    )]
    #[case(
        r#"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"#,
        9021
    )]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let (map, _initial_location, directions) = parse(input);

        let actual = part_two(&map, directions);

        assert_eq!(actual, expected);
    }
}
