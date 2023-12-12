#[derive(Debug, Clone, PartialEq)]
pub enum Tile {
    Operational,
    Damaged,
    Unknown,
}

impl Tile {
    pub fn from_char(chr: char) -> Option<Self> {
        match chr {
            '.' | '0' => Some(Self::Operational), // '0' for converting from binary
            '#' | '1' => Some(Self::Damaged),     // '1' for the same reason
            '?' => Some(Self::Unknown),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub tiles: Vec<Tile>,
    pub groups: Vec<usize>,
}

impl Record {
    pub fn from_line(line: &str) -> Self {
        let mut tiles_and_groups = line.split(' ');

        let tiles = tiles_and_groups
            .next()
            .expect("must contain a string of tiles")
            .chars()
            .map(|c| {
                Tile::from_char(c).unwrap_or_else(|| panic!("invalid tile: {c} detected in line"))
            })
            .collect::<Vec<Tile>>();

        let groups = tiles_and_groups
            .next()
            .expect("must contain a string of numbers of damaged tile groups")
            .split(',')
            .map(|c| {
                c.parse::<usize>()
                    .expect("each group number must be a valid number")
            })
            .collect::<Vec<usize>>();

        Self { tiles, groups }
    }

    fn calculate_unknown_positions(&self) -> Vec<usize> {
        self.tiles
            .iter()
            .enumerate()
            .filter_map(|(pos, tile)| {
                if *tile == Tile::Unknown {
                    Some(pos)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>()
    }

    fn calculate_permutations(&self, num_positions: usize) -> Vec<Vec<Tile>> {
        let total_damaged: usize = self.groups.iter().sum();
        let known_damaged = self
            .tiles
            .iter()
            .filter(|t| *t == &Tile::Damaged)
            .collect::<Vec<&Tile>>()
            .len();
        let required_damaged = total_damaged - known_damaged;

        let num = vec!['1'; num_positions].iter().collect::<String>();

        let mut num = isize::from_str_radix(&num, 2).unwrap();

        let mut permutations: Vec<Vec<Tile>> = Vec::with_capacity(num as usize);

        while num >= 0 {
            // left-0-padded binary string representation of `num`
            let bin_str = format!("{:0width$b}", num, width = num_positions);
            let tiles = Self::convert_binary_to_tiles(&bin_str);
            let num_damaged: usize = tiles
                .iter()
                .filter(|t| *t == &Tile::Damaged)
                .collect::<Vec<&Tile>>()
                .len();

            if num_damaged == required_damaged {
                permutations.push(tiles);
            }

            num -= 1;
        }

        permutations
    }

    fn convert_binary_to_tiles(bin_str: &str) -> Vec<Tile> {
        bin_str
            .chars()
            .filter_map(Tile::from_char)
            .collect::<Vec<Tile>>()
    }

    pub fn find_damaged_groups(tiles: &[Tile]) -> Vec<usize> {
        let mut groups: Vec<usize> = Vec::new();

        let mut damaged_cnt = 0;
        for tile in tiles {
            if tile == &Tile::Damaged {
                damaged_cnt += 1;
            }

            if damaged_cnt > 0 && tile != &Tile::Damaged {
                groups.push(damaged_cnt);
                damaged_cnt = 0;
            }
        }

        // count the last group
        if damaged_cnt > 0 {
            groups.push(damaged_cnt);
        }

        groups
    }

    pub fn filter_valid_permutations(&self) -> Vec<Vec<Tile>> {
        let unknown_positions = self.calculate_unknown_positions();
        // dbg!(&unknown_positions);
        let permutations = self.calculate_permutations(unknown_positions.len());
        // dbg!(&permutations);

        let mut valid_permutations: Vec<Vec<Tile>> = Vec::with_capacity(permutations.len());

        for perm in permutations.iter() {
            let mut filled_record = self.tiles.clone();

            for (index, pos) in unknown_positions.iter().enumerate() {
                filled_record[*pos] = perm[index].clone();
            }

            let damaged_groups = Self::find_damaged_groups(&filled_record[..]);
            // dbg!(&damaged_groups);
            if damaged_groups == self.groups {
                valid_permutations.push(perm.clone());
            }
        }

        // dbg!(&valid_permutations);
        valid_permutations
    }

#[cfg(test)]
mod tests {
    use std::iter::zip;

    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("#.?## 1,3", Record { tiles: vec![Tile::Damaged, Tile::Operational, Tile::Unknown, Tile::Damaged, Tile::Damaged], groups: vec![1, 3] })]
    fn parses_line_correctly(#[case] line: &str, #[case] expected: Record) {
        assert_eq!(Record::from_line(line), expected);
    }

    #[rstest]
    #[case(vec![Tile::Damaged, Tile::Operational, Tile::Damaged, Tile::Damaged, Tile::Operational], vec![1, 2])]
    #[case(vec![Tile::Operational, Tile::Damaged, Tile::Damaged, Tile::Damaged, Tile::Operational], vec![3])]
    #[case(vec![Tile::Operational, Tile::Operational, Tile::Operational, Tile::Operational, Tile::Operational], vec![])]
    #[case(vec![Tile::Damaged, Tile::Operational, Tile::Damaged, Tile::Operational, Tile::Damaged], vec![1, 1, 1])]
    fn finds_groups_correctly(#[case] tiles: Vec<Tile>, #[case] expected: Vec<usize>) {
        assert_eq!(Record::find_damaged_groups(&tiles[..]), expected);
    }

    #[rstest]
    fn filters_valid_permutations_correctly() {
        let input: &str = "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1";
        let records = input
            .lines()
            .map(Record::from_line)
            .collect::<Vec<Record>>();
        let expected: Vec<usize> = vec![1, 4, 1, 1, 4, 10];

        for (record, expected) in zip(records, expected) {
            assert_eq!(record.filter_valid_permutations().len(), expected);
        }
    }
}
