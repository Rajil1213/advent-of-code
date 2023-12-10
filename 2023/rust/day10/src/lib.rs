use std::collections::HashMap;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Position(usize, usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Kind {
    Vertical(Position),
    Horizontal(Position),
    L(Position),
    J(Position),
    Seven(Position),
    F(Position),
    Dot(Position),
    S(Position),
}

impl Kind {
    pub fn from(chr: &char, position: Position) -> Kind {
        match chr {
            '|' => Kind::Vertical(position),
            '-' => Kind::Horizontal(position),
            'L' => Kind::L(position),
            'J' => Kind::J(position),
            '7' => Kind::Seven(position),
            'F' => Kind::F(position),
            '.' => Kind::Dot(position),
            'S' => Kind::S(position),
            _ => panic!("Unparsable character: {chr}"),
        }
    }

    pub fn is_connected_to(&self, other: &Self) -> bool {
        match self {
            // vertical matches a node if they line up vertically, and one apart horizontally
            Self::Vertical(Position(x, y)) => match other {
                Self::L(Position(other_x, other_y)) => {
                    y == other_y && x.saturating_sub(*other_x).eq(&1)
                } // | above L
                Self::J(Position(other_x, other_y)) => {
                    y == other_y && x.saturating_sub(*other_x).eq(&1)
                } // | above J
                Self::Seven(Position(other_x, other_y)) => {
                    y == other_y && other_x.saturating_sub(*x).eq(&1)
                } // 7 above |
                Self::F(Position(other_x, other_y)) => {
                    y == other_y && other_x.saturating_sub(*x).eq(&1)
                } // F above |
                _ => false,
            },
            // Horizontal matches a node if they line up horizontally, and one apart vertically
            Self::Horizontal(Position(x, y)) => match other {
                Self::L(Position(other_x, other_y)) => {
                    x == other_x && other_y.saturating_sub(*y).eq(&1)
                } // L-
                Self::J(Position(other_x, other_y)) => {
                    x == other_x && y.saturating_sub(*other_y).eq(&1)
                } // -J
                Self::Seven(Position(other_x, other_y)) => {
                    x == other_x && y.saturating_sub(*other_y).eq(&1)
                } // -7
                Self::F(Position(other_x, other_y)) => {
                    x == other_x && other_y.saturating_sub(*other_y).eq(&1)
                } // F-
                _ => false,
            },
            // the remaining two are trickier, plot graphically to understand
            Self::L(Position(x, y)) => match other {
                Self::Seven(Position(other_x, other_y)) => {
                    (y == other_y && other_x.saturating_sub(*x).eq(&1))
                        || (x == other_x && other_y.saturating_sub(*y).eq(&0))
                } // 7 above L or L7
                Self::F(Position(other_x, other_y)) => {
                    y == other_y && other_x.saturating_sub(*x).eq(&1)
                } // F above L
                _ => false,
            },
            Self::J(Position(x, y)) => match other {
                Self::Seven(Position(other_x, other_y)) => {
                    y == other_y && other_x.saturating_sub(*x).eq(&1)
                } // 7 above J
                Self::F(Position(other_x, other_y)) => {
                    (y == other_y && other_x.saturating_sub(*x).eq(&1))
                        || (x == other_x && y.saturating_sub(*other_y).eq(&1))
                } // F above J or FJ
                _ => false,
            },
            Self::Dot(_pos) => false,
            Self::S(Position(x, y)) => match other {
                Self::Horizontal(Position(other_x, other_y)) => {
                    x.eq(other_x) && y.abs_diff(*other_y).eq(&1)
                } // -S-
                Self::Vertical(Position(other_x, other_y)) => {
                    y.eq(other_y) && x.abs_diff(*other_x).eq(&1)
                } // | above S or S above |
                Self::L(Position(other_x, other_y)) => {
                    (y == other_y && other_x.saturating_sub(*x).eq(&1))
                        || (x == other_x && other_y.saturating_sub(*y).eq(&0))
                } // S above L or L7
                Self::J(Position(other_x, other_y)) => {
                    (y == other_y && other_x.saturating_sub(*x).eq(&1))
                        || (x == other_x && y.saturating_sub(*other_y).eq(&1))
                } // S above J or SJ
                Self::Seven(Position(other_x, other_y)) => {
                    (x.eq(other_x) && other_y.saturating_sub(*y).eq(&1))
                        || (y.eq(other_y) && other_x.saturating_sub(*x).eq(&1))
                } // S7 or 7 above S
                Self::F(Position(other_x, other_y)) => {
                    (x.eq(other_x) && y.saturating_sub(*other_y).eq(&1))
                        || (y.eq(other_y) && x.saturating_sub(*other_x).eq(&1))
                } // FS or F above S
                _ => false,
            },
            // we've written up the logic for one side, for the reverse side, do the reverse
            _ => other.is_connected_to(self),
        }
    }

    pub fn position(&self) -> &Position {
        match self {
            Self::Horizontal(pos) => pos,
            Kind::Vertical(pos) => pos,
            Kind::L(pos) => pos,
            Kind::J(pos) => pos,
            Kind::Seven(pos) => pos,
            Kind::F(pos) => pos,
            Kind::Dot(pos) => pos,
            Kind::S(pos) => pos,
        }
    }
}

#[derive(Debug, Hash, Clone)]
pub struct Node {
    pub kind: Kind,
    pub visited: bool,
    pub explored: bool,
}

impl Node {
    pub fn neighbors(&self, graph: &HashMap<Position, Node>, max_pos: Position) -> Vec<Node> {
        let Position(x, y) = self.kind.position();
        let Position(max_x, max_y) = max_pos;
        let current_kind = &graph
            .get(&Position(*x, *y))
            .unwrap_or_else(|| panic!("{x},{y} must exist"))
            .kind;

        let (up, down, left, right) = (
            Position(if x + 1 > max_x { max_x - 1 } else { x + 1 }, *y),
            Position(x.saturating_sub(1), *y),
            Position(*x, y.saturating_sub(1)),
            Position(*x, if y + 1 > max_y { max_y - 1 } else { y + 1 }),
        );

        let mut neighbors: Vec<Node> = Vec::with_capacity(4);

        let up_node = graph
            .get(&up)
            .unwrap_or_else(|| panic!("{up:?} must exist in graph"));
        if up_node.kind.is_connected_to(current_kind) {
            neighbors.push(up_node.clone());
        }

        let down_node = graph
            .get(&down)
            .unwrap_or_else(|| panic!("{down:?} must exist in graph"));
        if down_node.kind.is_connected_to(current_kind) {
            neighbors.push(down_node.clone());
        }

        let left_node = graph
            .get(&left)
            .unwrap_or_else(|| panic!("{left:?} must exist in graph"));
        if left_node.kind.is_connected_to(current_kind) {
            neighbors.push(left_node.clone());
        }

        let right_node = graph
            .get(&right)
            .unwrap_or_else(|| panic!("{right:?} must exist in graph"));
        if right_node.kind.is_connected_to(current_kind) {
            neighbors.push(right_node.clone());
        }

        neighbors
    }
}

pub fn load_graph(input: &str) -> HashMap<Position, Node> {
    todo!();
}

pub fn create_adjacency_graph(graph: &HashMap<Position, Node>) -> HashMap<Node, Vec<Node>> {
    todo!();
}
