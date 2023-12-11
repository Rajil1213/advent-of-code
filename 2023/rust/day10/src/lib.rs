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
                    matches!(other, Self::Vertical(_) | Self::Seven(_) | Self::F(_))
                }
                Direction::South => matches!(other, Self::Vertical(_) | Self::J(_) | Self::L(_)),
                _ => false,
            },
            Node::Horizontal(_) => match direction {
                Direction::East => {
                    matches!(other, Self::Horizontal(_) | Self::J(_) | Self::Seven(_))
                }
                Direction::West => matches!(other, Self::Horizontal(_) | Self::F(_) | Self::L(_)),
                _ => false,
            },
            Node::L(_) => match direction {
                Direction::North => {
                    matches!(other, Self::Vertical(_) | Self::Seven(_) | Self::F(_))
                }
                Direction::East => {
                    matches!(other, Self::Horizontal(_) | Self::J(_) | Self::Seven(_))
                }
                _ => false,
            },
            Node::J(_) => match direction {
                Direction::North => {
                    matches!(other, Self::Vertical(_) | Self::Seven(_) | Self::F(_))
                }
                Direction::West => matches!(other, Self::Horizontal(_) | Self::F(_) | Self::L(_)),
                _ => false,
            },
            Node::Seven(_) => match direction {
                Direction::South => matches!(other, Self::Vertical(_) | Self::J(_) | Self::L(_)),
                Direction::West => matches!(other, Self::Horizontal(_) | Self::F(_) | Self::L(_)),
                _ => false,
            },
            Node::F(_) => match direction {
                Direction::South => matches!(other, Self::Vertical(_) | Self::J(_) | Self::L(_)),
                Direction::East => {
                    matches!(other, Self::Horizontal(_) | Self::J(_) | Self::Seven(_))
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
        let current_node = &graph[x * max_x + y];
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
    explored: bool,
}

pub fn create_adjacency_graph(graph: &[Node], max_pos: Position) -> HashMap<Node, AdjacencyInfo> {
    println!("Building adjacency graph from given graph");
    let mut adj_graph: HashMap<Node, AdjacencyInfo> = HashMap::new();

    for node in graph.iter() {
        let neighbors = node.neighbors(graph, max_pos.clone());
        adj_graph.entry(node.clone()).or_insert(AdjacencyInfo {
            neighbors,
            explored: false,
        });
    }

    adj_graph
}

pub fn find_cycle_length(
    starting_node: &Node,
    parent_node: Option<&Node>,
    current_node: Option<&Node>,
    distance: usize,
    adj_graph: &mut HashMap<Node, AdjacencyInfo>,
) -> Option<usize> {
    let mut local_distance = distance + 1;
    let current_node = current_node.unwrap_or(starting_node);
    let adjacency_info = adj_graph.get(current_node).unwrap_or_else(|| {
        panic!(
            "Node {:?} must be present in adj_graph",
            starting_node.position()
        )
    });

    if adjacency_info.explored {
        return None;
    }

    let neighbors = adjacency_info.neighbors.clone();

    for neighbor in neighbors.iter() {
        if parent_node.is_some() && neighbor == parent_node.unwrap() {
            continue;
        }

        if neighbor == starting_node {
            println!("Found starting_node again at {:?}", current_node);
            return Some(distance);
        }

        // go deeper
        let distance_to_cycle = find_cycle_length(
            starting_node,
            Some(current_node),
            Some(neighbor),
            local_distance,
            adj_graph,
        );
        if distance_to_cycle.is_some() {
            return distance_to_cycle;
        }

        // if you haven't found it yet, you haven't travelled
        local_distance = distance + 1;
    }

    // when we have inspect all of the current node's neighbors, it becomes explored
    adj_graph
        .entry(current_node.clone())
        .and_modify(|v| v.explored = true);

    // return original distance because none of the neighbors lead to the destination
    Some(distance)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_cycle_length_correctly() {
        // Case 1:
        let input: &str = "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ";
        let expected: usize = 15;
        let (graph, max_pos) = load_graph(input);
        let mut adj_graph = create_adjacency_graph(&graph, max_pos);

        let starting_node = find_starting_node(&graph);

        assert_eq!(
            find_cycle_length(starting_node, None, None, 0, &mut adj_graph),
            Some(expected)
        );

        // Case 2:
        let input: &str = "-L|F7
7S-7|
L|7||
-L-J|
L|-JF";
        let expected: usize = 7;
        let (graph, max_pos) = load_graph(input);
        let mut adj_graph = create_adjacency_graph(&graph, max_pos);
        let starting_node = find_starting_node(&graph);

        assert_eq!(
            find_cycle_length(starting_node, None, None, 0, &mut adj_graph),
            Some(expected)
        );
    }
}
