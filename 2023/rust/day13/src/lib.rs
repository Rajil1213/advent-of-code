use std::iter::zip;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element {
    Ash,
    Rock,
}

impl Element {
    pub fn from_char(c: char) -> Self {
        match c {
            '.' => Self::Ash,
            '#' => Self::Rock,
            _ => panic!("Invalid element: {c}"),
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            Self::Rock => "#",
            Self::Ash => ".",
        }
    }
}

pub type Pattern = Vec<Vec<Element>>;

pub fn parse_contents(contents: &str) -> Vec<Pattern> {
    let mut patterns: Vec<Pattern> = vec![];

    let mut pattern: Pattern = vec![];
    for line in contents.lines() {
        if line.trim().is_empty() {
            patterns.push(pattern);
            pattern = vec![];
            continue;
        }

        let mut pattern_line: Vec<Element> = Vec::with_capacity(line.len());

        for chr in line.chars() {
            pattern_line.push(Element::from_char(chr));
        }

        pattern.push(pattern_line);
    }

    if !pattern.is_empty() {
        patterns.push(pattern);
    }

    patterns
}

pub fn print_pattern(pat: &Pattern) {
    for i in pat {
        for j in i {
            print!("{}", j.to_str());
        }
        println!();
    }
}

pub fn find_horizontal_reflection_line(pat: &Pattern, smudge: bool) -> Option<usize> {
    let length = pat.len();
    for reflect_point in 1..length {
        let width = if reflect_point > length - reflect_point {
            length - reflect_point
        } else {
            reflect_point
        };
        let upper = &pat[reflect_point - width..reflect_point];
        let mut lower = pat[reflect_point..reflect_point + width].to_vec();
        lower.reverse();

        if smudge {
            if upper != lower && fix_smudge(&upper.to_vec(), &lower) {
                return Some(reflect_point);
            }
        } else if upper == lower {
            return Some(reflect_point);
        }
    }

    None
}

pub fn find_vertical_reflection_line(pat: &Pattern, smudge: bool) -> Option<usize> {
    let transposed_pattern = transpose_pattern(pat);
    find_horizontal_reflection_line(&transposed_pattern, smudge)
}

pub fn transpose_pattern(pat: &Pattern) -> Pattern {
    let mut transposed_pattern: Pattern = vec![];

    let col_count = pat[0].len();
    let mut col_num = 0;

    while col_num < col_count {
        let mut pattern_line: Vec<Element> = vec![];
        for line in pat {
            pattern_line.push(line[col_num].clone());
        }
        transposed_pattern.push(pattern_line);
        col_num += 1;
    }

    transposed_pattern
}

pub fn fix_smudge(upper: &Pattern, lower: &Pattern) -> bool {
    let mut differences = 0;
    for (upper_row, lower_row) in zip(upper, lower) {
        for (upper_elem, lower_elem) in zip(upper_row, lower_row) {
            if upper_elem != lower_elem {
                differences += 1;
                if differences > 1 {
                    return false;
                }
            }
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_horizontal_correctly() {
        let input: &str = "#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#";
        let patterns = parse_contents(input);

        assert_eq!(
            find_horizontal_reflection_line(&patterns[0], false),
            Some(4)
        );
    }

    #[test]
    fn find_vertical_correctly() {
        let input: &str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.";
        let patterns = parse_contents(input);

        assert_eq!(find_vertical_reflection_line(&patterns[0], false), Some(5));
    }

    #[test]
    fn find_horizontal_correctly_with_smudge() {
        let input: &str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.";
        let patterns = parse_contents(input);

        assert_eq!(find_horizontal_reflection_line(&patterns[0], true), Some(3));
    }

    #[test]
    fn find_vertical_correctly_with_smudge() {
        let input: &str = "#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#";
        let patterns = parse_contents(input);

        assert_eq!(
            find_vertical_reflection_line(&transpose_pattern(&patterns[0]), true),
            Some(1)
        );
    }
}
