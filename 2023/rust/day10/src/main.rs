use std::fs;

use day10::{
    calculate_double_loop_area, calculate_inner_tiles, create_adjacency_graph, find_cycle_path,
    find_starting_node, load_graph,
};

const INPUT_DIR: &str = "input";

fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
    part_two(&format!("{INPUT_DIR}/part_two.txt"));
}

fn part_one(path: &str) {
    let content = fs::read_to_string(path).expect("input file must be readable");

    let (graph, max_pos) = load_graph(&content);
    let mut adj_graph = create_adjacency_graph(&graph, max_pos);
    let starting_node = find_starting_node(&graph);

    let cycle_length = find_cycle_path(starting_node, &mut adj_graph).len();

    let furthest_distance = if cycle_length % 2 == 0 {
        cycle_length / 2
    } else {
        // minus because path can have one element extra -- the starting node itself
        (cycle_length - 1) / 2
    };

    println!("Furthest distance in the cycle = {furthest_distance}");
}

fn part_two(path: &str) {
    let content = fs::read_to_string(path).expect("input file must be readable");
    let (graph, max_pos) = load_graph(&content);
    let mut adj_graph = create_adjacency_graph(&graph, max_pos);
    let starting_node = find_starting_node(&graph);

    let cycle = find_cycle_path(starting_node, &mut adj_graph);
    let double_area = calculate_double_loop_area(&cycle);
    let num_inner_tiles = calculate_inner_tiles(double_area, cycle.len());

    println!("Number of inner tiles = {num_inner_tiles}");
}
