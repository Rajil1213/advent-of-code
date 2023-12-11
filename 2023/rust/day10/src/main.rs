use std::fs;

use day10::{create_adjacency_graph, find_cycle_length, find_starting_node, load_graph};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let content = fs::read_to_string(path).expect("input file must be readable");

    let (graph, max_pos) = load_graph(&content);
    let mut adj_graph = create_adjacency_graph(&graph, max_pos);
    let starting_node = find_starting_node(&graph);

    let cycle_length = find_cycle_length(starting_node, None, None, 0, &mut adj_graph);

    println!("Length of cycle = {cycle_length:?}");

    let cycle_length = cycle_length.expect("No cycle length found");

    let furthest_distance = if cycle_length % 2 == 0 {
        cycle_length / 2
    } else {
        (cycle_length + 1) / 2
    };

    println!("Furthest distance in the cycle = {furthest_distance}");
}
