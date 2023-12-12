use std::fs;

use day12::Record;

const INPUT_DIR: &str = "input";
fn main() {
    part_one(&format!("{INPUT_DIR}/part_one.txt"));
}

fn part_one(path: &str) {
    let content = fs::read_to_string(path).expect("input file must be readable");

    let records = content
        .lines()
        .map(Record::from_line)
        .collect::<Vec<Record>>();

    let mut sum_of_perms = 0;
    for (i, record) in records.iter().enumerate() {
        let value = record.filter_valid_permutations().len();
        dbg!(i + 1, value);
        sum_of_perms += value;
    }

    println!("Sum of all possible permutations = {sum_of_perms}");
}
