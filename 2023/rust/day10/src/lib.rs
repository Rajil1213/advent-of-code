use std::collections::HashMap;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Position(usize, usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Node {
    Vertical(Position),
    Horizontal(Position),
    L(Position),
    J(Position),
    Seven(Position),
    F(Position),
    Dot(Position),
    S(Position),
}

#[derive(Debug)]
pub enum Direction {
    North,
    South,
    East,
    West,
}

impl Node {
    pub fn from(chr: &char, position: Position) -> Node {
        match chr {
            '|' => Node::Vertical(position),
            '-' => Node::Horizontal(position),
            'L' => Node::L(position),
            'J' => Node::J(position),
            '7' => Node::Seven(position),
            'F' => Node::F(position),
            '.' => Node::Dot(position),
            'S' => Node::S(position),
            _ => panic!("Unparsable character: {chr}"),
        }
    }

    pub fn is_connected_to(&self, other: &Self, direction: &Direction) -> bool {
        if self == other {
            return false;
        }

        match self {
            Node::S(_) => match direction {
                Direction::North => {
                    matches!(other, Self::Vertical(_) | Self::Seven(_) | Self::F(_))
                }
                Direction::South => matches!(other, Self::Vertical(_) | Self::J(_) | Self::L(_)),
                Direction::East => {
                    matches!(other, Self::Horizontal(_) | Self::J(_) | Self::Seven(_))
                }
                Direction::West => matches!(other, Self::Horizontal(_) | Self::F(_) | Self::L(_)),
            },
            Node::Vertical(_) => match direction {
                Direction::North => {
                    matches!(
                        other,
                        Self::Vertical(_) | Self::Seven(_) | Self::F(_) | Self::S(_)
                    )
                }
                Direction::South => matches!(
                    other,
                    Self::Vertical(_) | Self::J(_) | Self::L(_) | Self::S(_)
                ),
                _ => false,
            },
            Node::Horizontal(_) => match direction {
                Direction::East => {
                    matches!(
                        other,
                        Self::Horizontal(_) | Self::J(_) | Self::Seven(_) | Self::S(_)
                    )
                }
                Direction::West => matches!(
                    other,
                    Self::Horizontal(_) | Self::F(_) | Self::L(_) | Self::S(_)
                ),
                _ => false,
            },
            Node::L(_) => match direction {
                Direction::North => {
                    matches!(
                        other,
                        Self::Vertical(_) | Self::Seven(_) | Self::F(_) | Self::S(_)
                    )
                }
                Direction::East => {
                    matches!(
                        other,
                        Self::Horizontal(_) | Self::J(_) | Self::Seven(_) | Self::S(_)
                    )
                }
                _ => false,
            },
            Node::J(_) => match direction {
                Direction::North => {
                    matches!(
                        other,
                        Self::Vertical(_) | Self::Seven(_) | Self::F(_) | Self::S(_)
                    )
                }
                Direction::West => matches!(
                    other,
                    Self::Horizontal(_) | Self::F(_) | Self::L(_) | Self::S(_)
                ),
                _ => false,
            },
            Node::Seven(_) => match direction {
                Direction::South => matches!(
                    other,
                    Self::Vertical(_) | Self::J(_) | Self::L(_) | Self::S(_)
                ),
                Direction::West => matches!(
                    other,
                    Self::Horizontal(_) | Self::F(_) | Self::L(_) | Self::S(_)
                ),
                _ => false,
            },
            Node::F(_) => match direction {
                Direction::South => matches!(
                    other,
                    Self::Vertical(_) | Self::J(_) | Self::L(_) | Self::S(_)
                ),
                Direction::East => {
                    matches!(
                        other,
                        Self::Horizontal(_) | Self::J(_) | Self::Seven(_) | Self::S(_)
                    )
                }
                _ => false,
            },
            Node::Dot(_) => false,
        }
    }

    pub fn position(&self) -> &Position {
        match self {
            Self::Horizontal(pos) => pos,
            Node::Vertical(pos) => pos,
            Node::L(pos) => pos,
            Node::J(pos) => pos,
            Node::Seven(pos) => pos,
            Node::F(pos) => pos,
            Node::Dot(pos) => pos,
            Node::S(pos) => pos,
        }
    }

    pub fn neighbors(&self, graph: &[Node], max_pos: Position) -> Vec<Node> {
        let Position(x, y) = self.position();
        let Position(max_x, max_y) = max_pos;
        let current_node = &graph[x * max_y + y];
        // dbg!(current_node);

        let (max_x, max_y) = (max_x as isize, max_y as isize);

        let relative_positions: [(isize, isize, Direction); 4] = [
            (1, 0, Direction::South),
            (-1, 0, Direction::North),
            (0, 1, Direction::East),
            (0, -1, Direction::West),
        ];

        let mut neighbors: Vec<Node> = vec![];

        for (delta_x, delta_y, dir) in relative_positions.iter() {
            let mut new_position_x = *x as isize + delta_x;
            let mut new_position_y = *y as isize + delta_y;

            if new_position_x == max_x {
                new_position_x = max_x - 1;
            }

            if new_position_x < 0 {
                new_position_x = 0;
            }

            if new_position_y < 0 {
                new_position_y = 0;
            }

            if new_position_y == max_y {
                new_position_y = max_y - 1;
            }

            let new_position_x = new_position_x as usize;
            let new_position_y = new_position_y as usize;

            let node_at_new_pos: &Node = &graph[new_position_x * max_y as usize + new_position_y];
            // dbg!(new_position_x, new_position_y, dir, node_at_new_pos);

            if current_node.is_connected_to(node_at_new_pos, dir) {
                neighbors.push(node_at_new_pos.clone());
            }
        }

        if neighbors.len().gt(&2) {
            println!("Spec violation: found node with more than two connections at ({x}, {y})");
        }

        neighbors
    }
}

pub fn load_graph(input: &str) -> (Vec<Node>, Position) {
    println!("Loading graph from input");
    let num_rows = input.lines().collect::<Vec<&str>>().len();
    let num_cols = input
        .lines()
        .next()
        .expect("there must be at least one row to compute no. of columns")
        .len();

    // let mut graph: Vec<Node> = Vec::with_capacity(num_rows.saturating_mul(num_cols));
    let mut graph: Vec<Node> = vec![];

    for (row, line) in input.lines().enumerate() {
        // dbg!(row);
        for (col, chr) in line.chars().enumerate() {
            // dbg!(col);
            let pos = Position(row, col);
            graph.push(Node::from(&chr, pos));
        }
    }

    (graph, Position(num_rows, num_cols))
}

#[derive(Debug)]
pub struct AdjacencyInfo {
    neighbors: Vec<Node>,
}

pub fn create_adjacency_graph(graph: &[Node], max_pos: Position) -> HashMap<Node, AdjacencyInfo> {
    println!("Building adjacency graph from given graph");
    let mut adj_graph: HashMap<Node, AdjacencyInfo> = HashMap::new();

    for node in graph.iter() {
        let neighbors = node.neighbors(graph, max_pos.clone());
        adj_graph
            .entry(node.clone())
            .or_insert(AdjacencyInfo { neighbors });
    }

    adj_graph
}

pub fn find_cycle_path(
    starting_node: &Node,
    adj_graph: &mut HashMap<Node, AdjacencyInfo>,
) -> Vec<Node> {
    let mut path: Vec<Node> = vec![];

    let mut current_node = starting_node;
    let mut parent_node: Option<&Node> = None;
    loop {
        let neighbors = &adj_graph
            .get(current_node)
            .unwrap_or_else(|| {
                panic!(
                    "Node {:?} must be present in adj_graph",
                    starting_node.position()
                )
            })
            .neighbors;

        // dbg!(&parent_node, &current_node, &neighbors);
        path.push(current_node.clone());

        // always returns one node (from two neighbors -- one of which is the `parent_node`)
        let next_node = neighbors
            .iter()
            .find(|n| parent_node.is_none() || *n != parent_node.unwrap())
            .expect("at least one node must be present");

        if next_node == starting_node {
            println!("Found starting_node again at {:?}", current_node);
            break;
        }

        parent_node = Some(current_node);
        current_node = next_node;
    }

    path
}

pub fn find_starting_node(graph: &[Node]) -> &Node {
    for value in graph.iter() {
        match value.clone() {
            Node::S(_pos) => return value,
            _ => continue,
        }
    }

    unreachable!("starting node must be present");
}

/// Reference: https://en.wikipedia.org/wiki/Shoelace_formula
pub fn calculate_double_loop_area(path: &[Node]) -> usize {
    let path_len = path.len();

    let mut area: isize = 0;
    for i in 0..path_len - 1 {
        let x_diff = path[i].position().0 as isize - path[i + 1].position().0 as isize;
        let y_sum = (path[i].position().1 + path[i + 1].position().1) as isize;
        area += x_diff * y_sum;
    }

    area.unsigned_abs()
}

/// Reference: https://en.wikipedia.org/wiki/Pick%27s_theorem
pub fn calculate_inner_tiles(double_area: usize, loop_length: usize) -> usize {
    (double_area + 2 - loop_length) / 2
}

#[cfg(test)]
mod tests {
    use std::iter::zip;

    use super::*;

    #[test]
    fn finds_cycle_length_correctly() {
        // Case 1:
        let input: &str = "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ";
        let expected: usize = 16;
        let (graph, max_pos) = load_graph(input);
        let mut adj_graph = create_adjacency_graph(&graph, max_pos);

        let starting_node = find_starting_node(&graph);

        let cycle = find_cycle_path(starting_node, &mut adj_graph);
        assert_eq!(cycle.len(), expected);

        // Case 2:
        let input: &str = "-L|F7
7S-7|
L|7||
-L-J|
L|-JF";
        let expected: usize = 8;
        let (graph, max_pos) = load_graph(input);
        let mut adj_graph = create_adjacency_graph(&graph, max_pos);
        let starting_node = find_starting_node(&graph);

        let cycle = find_cycle_path(starting_node, &mut adj_graph);
        // dbg!(&cycle);
        assert_eq!(cycle.len(), expected);
    }

    #[test]
    fn calculates_num_inner_tiles_correctly() {
        let inputs: Vec<&str> = vec![
            "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........",
            "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L",
            ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...",
        ];
        let expected_values: Vec<usize> = vec![4, 10, 8];

        for (input, expected) in zip(inputs, expected_values) {
            let (graph, max_pos) = load_graph(input);
            let mut adj_graph = create_adjacency_graph(&graph, max_pos);
            let starting_node = find_starting_node(&graph);

            let cycle = find_cycle_path(starting_node, &mut adj_graph);
            // dbg!(&cycle);
            let double_area = calculate_double_loop_area(&cycle);
            let num_inner_tiles = calculate_inner_tiles(double_area, cycle.len());

            assert_eq!(num_inner_tiles, expected);
        }
    }
}
