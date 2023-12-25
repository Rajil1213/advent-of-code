use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Operator {
    Greater,
    Less,
}

impl Operator {
    pub fn from_char(ch: &char) -> Self {
        match ch {
            '>' => Self::Greater,
            '<' => Self::Less,
            _ => panic!("Unknown operator: {ch} detected"),
        }
    }

    pub fn compare(&self, a: usize, b: usize) -> bool {
        match self {
            Self::Greater => a > b,
            Self::Less => a < b,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Rating {
    pub x: usize,
    pub m: usize,
    pub a: usize,
    pub s: usize,
}

impl Rating {
    pub fn from_line(line: &str) -> Self {
        let values = line
            .strip_suffix('}')
            .expect("each part line must end with }")
            .strip_prefix('{')
            .expect("each part line must begin with {");
        let values = values
            .split(',')
            .map(|kv| {
                kv.split('=').collect::<Vec<&str>>()[1]
                    .parse::<usize>()
                    .expect("a number must be present after =")
            })
            .collect::<Vec<usize>>();

        assert!(values.len().eq(&4));

        Self {
            x: values[0],
            m: values[1],
            a: values[2],
            s: values[3],
        }
    }
}

#[derive(Debug, Clone)]
pub enum Status {
    Accepted,
    Rejected,
}

impl Status {
    pub fn from_workflow(s: &str) -> Option<Self> {
        match s {
            "A" => Some(Self::Accepted),
            "R" => Some(Self::Rejected),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Part {
    pub rating: Rating,
    pub status: Option<Status>,
}

impl Part {
    pub fn from(line: &str) -> Self {
        Self {
            rating: Rating::from_line(line),
            status: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Threshold {
    X(usize),
    M(usize),
    A(usize),
    S(usize),
}

impl Threshold {
    pub fn from_char_and_value(ch: &char, value: usize) -> Self {
        match ch {
            'x' => Self::X(value),
            'm' => Self::M(value),
            'a' => Self::A(value),
            's' => Self::S(value),
            _ => panic!("invalid metric {ch} detected"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub threshold: Threshold,
    pub operator: Operator,
    pub target: String,
}

impl Rule {
    pub fn from_clause(clause: &str) -> Self {
        let condition_and_dest = clause.split(':').collect::<Vec<&str>>();
        assert!(condition_and_dest.len().eq(&2));

        let mut condition = condition_and_dest[0].chars();

        let metric = condition
            .next()
            .expect("each rule must start with a metric");

        let operator = condition
            .next()
            .expect("the second char in the rule must be an operator");
        let operator = Operator::from_char(&operator);

        let value = condition
            .collect::<String>()
            .parse::<usize>()
            .expect("rest of the rule must be a valid number");

        let threshold = Threshold::from_char_and_value(&metric, value);

        Self {
            threshold,
            operator,
            target: condition_and_dest[1].to_string(),
        }
    }

    pub fn compare(&self, part: &Part) -> bool {
        match self.threshold {
            Threshold::X(value) => self.operator.compare(part.rating.x, value),
            Threshold::M(value) => self.operator.compare(part.rating.m, value),
            Threshold::A(value) => self.operator.compare(part.rating.a, value),
            Threshold::S(value) => self.operator.compare(part.rating.s, value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Workflow {
    pub name: String,
    pub rules: Vec<Rule>,
    pub final_dest: String,
}

impl Workflow {
    pub fn from_line(line: &str) -> Self {
        let name_and_rules = line.split('{').collect::<Vec<&str>>();
        assert!(
            name_and_rules.len().eq(&2),
            "line must contain name and rules separated by {{"
        );

        let name = name_and_rules[0].to_string();
        let rules = name_and_rules[1]
            .strip_suffix('}')
            .expect("line must end with }")
            .split(',');
        let final_dest = rules.clone().last().unwrap().to_string();

        let rules = rules.collect::<Vec<&str>>();
        let num_rules = rules.len() - 1; // all except the last one

        let mut parsed_rules: Vec<Rule> = Vec::with_capacity(num_rules);
        for clause in rules.iter().take(num_rules) {
            parsed_rules.push(Rule::from_clause(clause));
        }

        Self {
            name,
            rules: parsed_rules,
            final_dest,
        }
    }
}

pub fn parse(input: &str) -> (HashMap<String, Workflow>, Vec<Part>) {
    let mut lines = input.lines();

    // parse workflows
    let mut workflow_map: HashMap<String, Workflow> = HashMap::new();
    let mut line = lines.next().unwrap();
    while !line.is_empty() {
        let workflow = Workflow::from_line(line);
        workflow_map.insert(workflow.name.clone(), workflow);
        line = lines.next().unwrap();
    }

    // parse parts
    let mut parts: Vec<Part> = vec![];
    for line in lines {
        parts.push(Part::from(line));
    }

    (workflow_map, parts)
}

pub fn run(part: &Part, workflow_map: &HashMap<String, Workflow>) -> Option<Status> {
    const STARTING_WORKFLOW_NAME: &str = "in";

    let mut workflow_name = STARTING_WORKFLOW_NAME.to_string();
    let mut status = Status::from_workflow(&workflow_name);
    while status.is_none() {
        let workflow = workflow_map
            .get(&workflow_name)
            .expect("{workflow_name} must exist");

        let mut found = false;
        for rule in workflow.rules.iter() {
            if rule.compare(part) {
                workflow_name = rule.target.clone();
                status = Status::from_workflow(&workflow_name);
                found = true;
                break;
            }
        }

        if !found {
            workflow_name = workflow.final_dest.clone();
            status = Status::from_workflow(&workflow_name);
        }
    }

    status
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_accepted_parts_correctly() {
        let input: &str = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}";
        let (workflow_map, parts) = parse(input);

        let mut num_accepted = 0;
        let mut sum = 0;
        for part in parts.iter() {
            if let Some(Status::Accepted) = run(part, &workflow_map) {
                num_accepted += 1;
                sum += part.rating.x + part.rating.m + part.rating.a + part.rating.s;
            }
        }

        assert!(num_accepted.eq(&3));
        assert!(sum.eq(&19114));
    }
}
