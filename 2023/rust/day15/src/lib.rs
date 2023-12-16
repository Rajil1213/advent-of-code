use std::hash::{BuildHasher, Hasher};

#[derive(Default)]
pub struct AsciiHasher {
    state: u64,
}

impl Hasher for AsciiHasher {
    fn write(&mut self, bytes: &[u8]) {
        let mut current_value: u64 = 0;
        for chr in bytes {
            current_value += u64::from(*chr);
            current_value *= 17;
            current_value %= 256;
        }
        self.state = current_value;
    }

    fn finish(&self) -> u64 {
        self.state
    }
}

pub struct AocAsciiHasherBuilder;

impl BuildHasher for AocAsciiHasherBuilder {
    type Hasher = AsciiHasher;

    fn build_hasher(&self) -> Self::Hasher {
        AsciiHasher { state: 0 }
    }
}

pub fn sum_hashes(contents: &str) -> u64 {
    let mut sum: u64 = 0;

    for value in contents.split(',') {
        let mut hasher = AsciiHasher::default();
        let value_bytes = value.strip_suffix('\n').unwrap_or(value).as_bytes();
        hasher.write(value_bytes);
        let hashed_value = hasher.finish();
        println!(
            "{:?} => {hashed_value}",
            String::from_utf8(value_bytes.to_vec())
        );
        sum += hashed_value;
    }

    sum
}

#[cfg(test)]
mod tests {
    use std::hash::Hasher;

    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(b"rn=1", 30)]
    #[case(b"cm-", 253)]
    #[case(b"qp=3", 97)]
    #[case(b"ot=9", 9)]
    fn hashes_correctly(#[case] input: &[u8], #[case] expected: u64) {
        let mut hasher = AsciiHasher::default();
        hasher.write(input);

        assert_eq!(hasher.finish(), expected);
    }

    #[test]
    fn calculates_hash_sum_correctly() {
        let input: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
        let sum = sum_hashes(input);

        assert_eq!(sum, 1320);
    }
}
