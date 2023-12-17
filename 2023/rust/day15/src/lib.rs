use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Label(String);

impl FromStr for Label {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Label(s.to_string()))
    }
}

impl Label {
    pub fn hash(&self) -> usize {
        let mut current_value: usize = 0;
        for chr in self.0.as_bytes() {
            current_value += usize::from(*chr);
            current_value *= 17;
            current_value %= 256;
        }

        current_value
    }
}

pub fn sum_hashes(contents: &str) -> usize {
    let mut sum: usize = 0;

    for value in contents.split(',') {
        let label = Label::from_str(value.strip_suffix('\n').unwrap_or(value)).unwrap();
        let hashed_value = label.hash();

        sum += hashed_value;
    }

    sum
}

const MINUS: char = '-';

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lens {
    label: Label,
    focal_length: u8,
}

pub fn sum_focusing_power(boxes: &[Vec<Lens>]) -> usize {
    let mut sum = 0;
    for (box_num, v) in boxes.iter().enumerate() {
        // println!("{k:?} => {box_num}");

        for (i, lens) in v.iter().enumerate() {
            sum += (box_num + 1) * (i + 1) * usize::from(lens.focal_length);
        }
    }

    sum
}

pub fn form_boxes(contents: &str) -> Vec<Vec<Lens>> {
    let mut boxes: Vec<Vec<Lens>> = vec![vec![]; 256];
    let contents = contents.trim_end();
    for step in contents.split(',') {
        if step.ends_with(MINUS) {
            let label = Label::from_str(step.strip_suffix(MINUS).unwrap()).unwrap();
            let box_num = label.hash();
            if !boxes[box_num].is_empty() {
                boxes[box_num] = boxes[box_num]
                    .iter()
                    .filter_map(|lens| {
                        if lens.label == label {
                            return None;
                        }
                        Some(lens.clone())
                    })
                    .collect::<Vec<Lens>>()
                    .to_vec();
            }
        } else {
            // <label>=(1..=9)
            let label = Label::from_str(&step[..step.len() - 2]).unwrap();
            let focal_length = step
                .chars()
                .nth(step.len() - 1)
                .unwrap()
                .to_string()
                .parse::<u8>()
                .unwrap();
            let box_num = label.hash();

            if boxes[box_num].is_empty() {
                boxes[box_num].push(Lens {
                    label,
                    focal_length,
                });
            } else {
                let mut exists = false;
                boxes[box_num] = boxes[box_num]
                    .iter_mut()
                    .map(|l| {
                        if l.label == label {
                            l.focal_length = focal_length;
                            exists = true;
                        }
                        l.clone()
                    })
                    .collect::<Vec<Lens>>();

                if !exists {
                    boxes[box_num].push(Lens {
                        label,
                        focal_length,
                    });
                }
            }
        }
    }

    boxes
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("rn=1", 30)]
    #[case("cm-", 253)]
    #[case("qp=3", 97)]
    #[case("ot=9", 9)]
    #[case("ot", 3)]
    #[case("ab", 3)]
    #[case("rn", 0)]
    #[case("cm", 0)]
    fn hashes_correctly(#[case] input: &str, #[case] expected: usize) {
        let label = Label::from_str(input).unwrap();

        assert_eq!(label.hash(), expected);
    }

    #[test]
    fn calculates_hash_sum_correctly() {
        let input: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
        let sum = sum_hashes(input);

        assert_eq!(sum, 1320);
    }

    #[test]
    fn calculates_focusing_power_correctly() {
        let input: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
        let boxes = form_boxes(input);
        dbg!(&boxes);

        let focussing_power = sum_focusing_power(&boxes);

        assert_eq!(focussing_power, 145);
    }
}
