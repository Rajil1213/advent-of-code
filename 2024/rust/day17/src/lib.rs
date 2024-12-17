use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operand {
    Literal(usize),
    Register(RegisterKind),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Literal(v) => write!(f, "{v}"),
            Operand::Register(register_kind) => write!(f, "{register_kind}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegisterKind {
    A,
    B,
    C,
}

impl Display for RegisterKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterKind::A => write!(f, "A"),
            RegisterKind::B => write!(f, "B"),
            RegisterKind::C => write!(f, "C"),
        }
    }
}

impl Operand {
    fn from(value: char, is_combo: bool) -> Self {
        if is_combo {
            match value {
                '0'..='3' => Self::Literal(value.to_digit(10).unwrap() as usize),
                '4' => Self::Register(RegisterKind::A),
                '5' => Self::Register(RegisterKind::B),
                '6' => Self::Register(RegisterKind::C),
                _ => unreachable!("invalid operand: {value}"),
            }
        } else {
            Self::Literal(value.to_digit(10).unwrap() as usize)
        }
    }

    pub fn value(&self, registers: &HashMap<RegisterKind, usize>) -> usize {
        match self {
            Operand::Literal(val) => *val,
            Operand::Register(register_kind) => registers.get(register_kind).copied().unwrap_or(0),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction {
    Adv,
    Bxl,
    Bst,
    Jnz,
    Bxc,
    Out,
    Bdv,
    Cdv,
}

impl From<char> for Instruction {
    fn from(value: char) -> Self {
        match value {
            '0' => Self::Adv,
            '1' => Self::Bxl,
            '2' => Self::Bst,
            '3' => Self::Jnz,
            '4' => Self::Bxc,
            '5' => Self::Out,
            '6' => Self::Bdv,
            '7' => Self::Cdv,
            _ => unreachable!("invalid instruction: {value}"),
        }
    }
}

impl Instruction {
    pub fn operate(
        &self,
        char_val: char,
        registers: &mut HashMap<RegisterKind, usize>,
    ) -> (Option<usize>, Option<usize>) {
        match self {
            Instruction::Adv => {
                let operand = Operand::from(char_val, true);
                let op_value = operand.value(registers);
                let register_a = registers.get(&RegisterKind::A).copied().unwrap_or(0);
                let result = if (register_a as f64).log2() < op_value as f64 {
                    0
                } else {
                    register_a / (2u32.pow(op_value as u32) as usize)
                };

                registers
                    .entry(RegisterKind::A)
                    .and_modify(|v| *v = result)
                    .or_insert(0);

                // println!("A = A / 2**{operand}");

                (None, None)
            }

            Instruction::Bxl => {
                let op_value = Operand::from(char_val, false).value(registers);
                registers
                    .entry(RegisterKind::B)
                    .and_modify(|v| *v ^= op_value)
                    .or_insert(op_value);

                // println!("B = B ^ {op_value}");

                (None, None)
            }

            Instruction::Bst => {
                let operand = Operand::from(char_val, true);
                let op_value = operand.value(registers);
                registers
                    .entry(RegisterKind::B)
                    .and_modify(|v| *v = op_value % 8)
                    .or_insert(0);

                // println!("B = {operand} % 8");

                (None, None)
            }
            Instruction::Jnz => {
                let op_value = Operand::from(char_val, false).value(registers);
                let value_a = registers.get(&RegisterKind::A).unwrap_or(&0);

                if *value_a == 0 {
                    // println!("NOP");
                    return (None, None);
                }

                // println!("JNZ {op_value}");
                (None, Some(op_value))
            }
            Instruction::Bxc => {
                let register_c = registers.get(&RegisterKind::C).copied().unwrap_or(0);

                registers
                    .entry(RegisterKind::B)
                    .and_modify(|v| *v ^= register_c)
                    .or_insert(register_c);

                // println!("B = B ^ C");

                (None, None)
            }
            Instruction::Out => {
                let operand = Operand::from(char_val, true);
                let op_value = operand.value(registers);

                // println!("PRINT {operand} % 8");

                (Some(op_value % 8), None)
            }
            Instruction::Bdv => {
                let operand = Operand::from(char_val, true);
                let op_value = operand.value(registers);

                let register_a = registers.get(&RegisterKind::A).copied().unwrap_or(0);
                let result = if (register_a as f64).log2() < op_value as f64 {
                    0
                } else {
                    register_a / (2u32.pow(op_value as u32) as usize)
                };

                // println!("B = A / 2**{operand}");

                registers
                    .entry(RegisterKind::B)
                    .and_modify(|v| *v = result)
                    .or_insert(0);

                (None, None)
            }
            Instruction::Cdv => {
                let operand = Operand::from(char_val, true);
                let op_value = operand.value(registers);

                let register_a = registers.get(&RegisterKind::A).copied().unwrap_or(0);
                let result = if (register_a as f64).log2() < op_value as f64 {
                    0
                } else {
                    register_a / (2u32.pow(op_value as u32) as usize)
                };

                // println!("C = A / 2**{operand}");

                registers
                    .entry(RegisterKind::C)
                    .and_modify(|v| *v = result)
                    .or_insert(0);

                (None, None)
            }
        }
    }
}

pub fn parse(input: &str) -> (Vec<char>, HashMap<RegisterKind, usize>) {
    let mut lines = input.lines();

    let registers = [RegisterKind::A, RegisterKind::B, RegisterKind::C];

    let registers = registers
        .into_iter()
        .zip((0..registers.len()).map(|_| {
            lines
                .next()
                .unwrap()
                .split(":")
                .last()
                .unwrap()
                .trim()
                .parse::<usize>()
                .unwrap()
        }))
        .collect::<HashMap<RegisterKind, usize>>();

    lines.next().unwrap(); // skip blank

    let instructions = lines
        .next()
        .unwrap()
        .split_whitespace()
        .last()
        .unwrap()
        .split(",")
        .map(|c| 
            c.chars().next().unwrap()
        )
        .collect::<Vec<char>>();

    (instructions, registers)
}

pub fn part_one(program: &[char], registers: &mut HashMap<RegisterKind, usize>) -> String {
    let mut output = Vec::new();

    let num_instructions = program.len();

    let mut index = 0;
    loop {
        if index + 1 >= num_instructions {
            break;
        }

        let instruction = Instruction::from(program[index]);         
        let operand = program[index + 1];

        let (result, jump) = instruction.operate(operand, registers);

        if let Some(result) = result {
            output.push(result.to_string());
        }

        if let Some(jump) = jump {
            index = jump;
        } else {
            index += 2;
        }
    }

    output.join(",").to_string()
}

pub fn part_two(program: &[char]) -> usize {
    let expected = program.iter().collect::<String>();

    search_a(&expected, 1, expected.len() - 1).unwrap().parse().unwrap()
}

fn search_a(program: &str, candidate: usize, index: usize) -> Option<String> {
    let expected = program.chars().enumerate().filter_map(|(i, d)| if i >= index { Some(d) } else { None }).collect::<String>();

    for option in candidate..candidate+8 {
        let output = alternate_program(option);

        if output.to_string() == program {
            return Some(option.to_string());
        }

        if output == expected.parse::<usize>().unwrap() {
            if let Some(sol) = search_a(program, option * 8, index - 1) {
                return Some(sol);
            }
        }
    }

    None
}

fn alternate_program(a: usize) -> usize {
    let mut a = a;
    let mut result = 0;

    while a != 0 {
        let mut b = a % 8;
        b ^= 3;
        let c = a / 2u32.pow(b as u32) as usize;
        b ^= c; 
        b ^= 5;
        a /= 8;

        result = result * 10 + (b % 8);
    }

    result
} 

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(r#"Register A: 729
Register B: 0
Register C: 5

Program: 0,1,5,4,3,0"#, (
            vec!['0', '1', '5', '4', '3', '0'], 
            HashMap::from([(RegisterKind::A, 729), (RegisterKind::B, 0), (RegisterKind::C, 5)])
        )
    )]
    fn test_parse(
        #[case] input: &str,
        #[case] expected: (Vec<char>, HashMap<RegisterKind, usize>),
    ) {
        let (program, registers) = parse(input);

        assert_eq!(program, expected.0);
        assert_eq!(registers, expected.1);
    }

    #[rstest]
    #[case(r#"Register A: 0
Register B: 0
Register C: 9

Program: 2,6"#, "")]
    #[case(r#"Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4"#, "0,1,2")]
    #[case(r#"Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"#, "4,2,5,6,7,7,7,7,3,1,0")]
    #[case(r#"Register A: 0
Register B: 29
Register C: 0

Program: 1,7"#, "")]
    #[case(r#"Register A: 0
Register B: 2024
Register C: 43690

Program: 4,0"#, "")]
    #[case(r#"Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"#, "4,6,3,5,6,3,5,2,1,0")]
    fn test_part_one(#[case] input: &str, #[case] expected: &str) {
        let (program, mut registers) = parse(input);

        let actual = part_one(&program, &mut registers);
        dbg!(registers);

        // assert_eq!(actual.0, expected.to_string());
        assert_eq!(actual, expected.to_string());
    }

    #[rstest]
    #[case(r#"Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"#, 117440)]
    fn test_part_two(#[case] input: &str, #[case] expected: usize) {
        let (program, _registers) = parse(input);

        let actual = part_two(&program);

        assert_eq!(actual, expected);
    }
}
