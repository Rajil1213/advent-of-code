use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    pub fn to_point(&self) -> (isize, isize) {
        match self {
            Self::Up => (-1, 0),
            Self::Down => (1, 0),
            Self::Left => (0, -1),
            Self::Right => (0, 1),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TileType {
    EmptySpace,
    RightMirror,
    LeftMirror,
    VerticalSplitter,
    HorizontalSplitter,
}

impl TileType {
    pub fn from_char(c: char) -> Self {
        match c {
            '.' => Self::EmptySpace,
            '/' => Self::RightMirror,
            '\\' => Self::LeftMirror,
            '|' => Self::VerticalSplitter,
            '-' => Self::HorizontalSplitter,
            _ => panic!("Unusual tile: {c} detected"),
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            TileType::EmptySpace => '.',
            TileType::RightMirror => '/',
            TileType::LeftMirror => '\\',
            TileType::VerticalSplitter => '|',
            TileType::HorizontalSplitter => '-',
        }
    }

    pub fn outgoing_direction(&self, towards: &Direction) -> Vec<Direction> {
        match self {
            Self::EmptySpace => vec![towards.to_owned()],
            TileType::RightMirror => match towards {
                Direction::Left => vec![Direction::Down],
                Direction::Up => vec![Direction::Right],
                Direction::Right => vec![Direction::Up],
                Direction::Down => vec![Direction::Left],
            },
            TileType::LeftMirror => match towards {
                Direction::Up => vec![Direction::Left],
                Direction::Down => vec![Direction::Right],
                Direction::Left => vec![Direction::Up],
                Direction::Right => vec![Direction::Down],
            },
            TileType::VerticalSplitter => match towards {
                Direction::Up => vec![Direction::Up],
                Direction::Down => vec![Direction::Down],
                _ => vec![Direction::Up, Direction::Down],
            },
            TileType::HorizontalSplitter => match towards {
                Direction::Left => vec![Direction::Left],
                Direction::Right => vec![Direction::Right],
                _ => vec![Direction::Right, Direction::Left],
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tile {
    pub kind: TileType,
    pub energized: bool,
    pub directions: HashSet<Direction>,
}

impl Tile {
    pub fn from_char(c: char) -> Self {
        Self {
            kind: TileType::from_char(c),
            energized: false,
            directions: HashSet::new(),
        }
    }
}

pub type Grid = Vec<Vec<Tile>>;

pub fn parse(contents: &str) -> Grid {
    let lines = contents.lines().collect::<Vec<&str>>();
    let width = lines.len();

    let mut grid: Grid = Vec::with_capacity(width);

    for line in lines {
        let mut grid_line: Vec<Tile> = Vec::with_capacity(width);
        for ch in line.chars() {
            grid_line.push(Tile::from_char(ch));
        }

        grid.push(grid_line);
    }

    grid
}

pub fn traverse_grid(start: (usize, usize), towards: &Direction, grid: &mut Grid) {
    let (row, col) = start;

    let size = grid.len();
    // end of the road
    if row >= size || col >= size {
        return;
    }

    let tile = &mut grid[row][col];

    // if the beam has already traversed this tile along the same direction, no need to go there again
    if !tile.directions.insert(towards.clone()) {
        return;
    }

    tile.energized = true;

    let next_directions = tile.kind.outgoing_direction(towards);

    for direction in next_directions {
        let (delta_row, delta_col) = direction.to_point();

        let next_row = row as isize + delta_row;
        let next_col = col as isize + delta_col;

        if next_row < 0 || next_col < 0 {
            continue;
        }

        traverse_grid((next_row as usize, next_col as usize), &direction, grid);
    }
}

pub fn count_energized(grid: &Grid) -> usize {
    let mut count = 0;

    for grid_line in grid {
        for tile in grid_line {
            if tile.energized {
                count += 1;
            }
        }
    }

    count
}

pub fn print_grid(grid: &Grid) {
    for grid_line in grid {
        for tile in grid_line {
            let num_directions = tile.directions.len();
            match num_directions {
                1 => match tile.directions.iter().next().unwrap() {
                    Direction::Right => print!(">"),
                    Direction::Up => print!("^"),
                    Direction::Down => print!("v"),
                    Direction::Left => print!("<"),
                },
                0 => print!("{}", tile.kind.to_char()),
                _ => print!("{num_directions}"),
            }
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_energized_tiles_correctly() {
        let input: &str = ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....";
        let mut grid = parse(input);
        println!("Original Grid:");
        print_grid(&grid);

        traverse_grid((0, 0), &Direction::Right, &mut grid);
        println!("Traversed Grid:");
        print_grid(&grid);

        assert_eq!(count_energized(&grid), 46);

        let input: &str = "\\-..../................\\...../.
....\\.................\\........
.|./........................./.
.--...............-.|..........
............|......|.........|.
..|\\.....|................/....
.........................../...
.../.......|./-..\\.............
...-...\\......-.........../....
.|............-......../.......
.................\\/.........\\/.
......./|../..........\\/.......
....../........................
.\\.................|...........
.../................|........|.
|.........-\\...-.-......./.....
......|........./............\\.
...............................
....../.../......-.............
....................../\\/......
|.......\\......................
................../........-...
.............................|.
.\\.............-..-...|...-.-..
..-..../....\\..................
....-.........-.......\\......-.
..................../-.......|.
...............................
...............................
-.............|.-..............";

        let mut grid = parse(input);
        println!("Original Grid:");
        print_grid(&grid);

        traverse_grid((0, 0), &Direction::Right, &mut grid);
        println!("Traversed Grid");
        print_grid(&grid);

        assert_eq!(count_energized(&grid), 73);
    }
}
