const NUM_MAPS: i8 = 7;

#[derive(Debug, Clone, Copy)]
pub struct CategoryMap {
    pub src: usize,
    pub dst: usize,
    pub range: usize,
}

impl CategoryMap {
    pub fn get_value(&self, val: usize) -> Option<usize> {
        if val < self.src {
            return None;
        }

        let diff: usize = val - self.src;
        if diff < self.range {
            return Some(self.dst + diff);
        }

        None
    }
}

pub fn get_seeds(line: &str) -> Vec<usize> {
    line.split("seeds: ")
        .last()
        .expect("line must start with seeds:")
        .split(' ')
        .map(|v| v.parse::<usize>().expect("each seed must be a number"))
        .collect::<Vec<usize>>()
}

pub fn find_mapped_value(maps: &[Vec<CategoryMap>; NUM_MAPS as usize], seed: usize) -> usize {
    let mut value: usize = seed;
    for map in maps {
        value = map
            .iter()
            .find_map(|cm| cm.get_value(value))
            .unwrap_or(value);
    }

    value
}

pub fn parse_map_lines(lines: Vec<&str>) -> [Vec<CategoryMap>; NUM_MAPS as usize] {
    const CATEGORY_MAP_LIST: Vec<CategoryMap> = Vec::new();
    let mut maps: [Vec<CategoryMap>; NUM_MAPS as usize] = [CATEGORY_MAP_LIST; NUM_MAPS as usize];

    let mut map_index: i8 = -1; // so that first occurrence of "map:" causes index to be 1
    for line in lines {
        if line.ends_with("map:") {
            // if line ends with ":map", start a new map
            map_index += 1;
            if map_index.gt(&NUM_MAPS) {
                panic!("More than 8 maps found!");
            }
            continue;
        }

        if line.is_empty() {
            continue;
        }

        // if line does not begin with a number
        if !line.chars().next().unwrap().is_ascii_digit() {
            continue;
        }

        maps[map_index as usize].push(get_map(line));
    }

    maps
}

pub fn get_map(line: &str) -> CategoryMap {
    let values = line
        .split(' ')
        .map(|v| v.parse::<usize>().expect("each value must be a number"))
        .collect::<Vec<usize>>();
    assert!(values.len().eq(&3), "each line must have three values");

    let dst = values[0];
    let src = values[1];
    let range = values[2];

    CategoryMap { src, dst, range }
}

pub fn get_groups_of_two(input_vec: Vec<usize>) -> Vec<(usize, usize)> {
    let length = input_vec.len();
    assert_eq!(length % 2, 0);

    let mut grouped_vec: Vec<(usize, usize)> = Vec::with_capacity(length / 2);
    for i in (0..length).step_by(2) {
        grouped_vec.push((input_vec[i], input_vec[i + 1]))
    }

    grouped_vec
}

pub fn find_nearest_from_ranges(
    input: Vec<(usize, usize)>,
    maps: &[Vec<CategoryMap>; NUM_MAPS as usize],
) -> usize {
    let mut nearest_location = usize::MAX;

    for (starting, length) in input {
        for seed in starting..starting + length {
            let location = find_mapped_value(maps, seed);
            if location.lt(&nearest_location) {
                nearest_location = location;
            }
        }
    }

    nearest_location
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn computes_nearest_location_correctly() {
        let contents = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";

        let first_line = contents.lines().next().unwrap();
        let full_input = contents.lines().collect::<Vec<&str>>();

        let seeds = get_seeds(first_line);
        let maps = parse_map_lines(full_input);

        let mut min_location = usize::MAX;

        for seed in seeds {
            let location = find_mapped_value(&maps, seed);
            if location.lt(&min_location) {
                min_location = location;
            }
        }

        assert_eq!(min_location, 35);
    }

    #[test]
    fn find_nearest_location_from_range_correctly() {
        let contents = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";
        let first_line = contents.lines().next().unwrap();
        let full_input = contents.lines().collect::<Vec<&str>>();

        let seed_ranges = get_groups_of_two(get_seeds(first_line));
        let maps = parse_map_lines(full_input);

        let min_location = find_nearest_from_ranges(seed_ranges, &maps);

        assert_eq!(min_location, 46);
    }
}
