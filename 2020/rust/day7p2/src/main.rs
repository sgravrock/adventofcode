#[macro_use] extern crate lazy_static;
extern crate regex;
use regex::Regex;
use std::collections::HashSet;
use std::collections::HashMap;
mod input;

fn main() {
	 let result = can_contain_shiny_gold(parse_rules(input::puzzle_input()));
    println!("{}", result.len());
}

// Returns a map from bag color to what bags contain taht color,
// the opposite of the data structured described by the input.
fn parse_rules(input: &str) -> HashMap<String, Vec<String>> {
	let mut result = HashMap::new();

	for line in input.lines() {
		lazy_static! {
			static ref TOP_RE: Regex = Regex::new(
				r"(.+) bags contain (.+)\.$"
			).unwrap();
			static ref CONTENTS_RE: Regex = Regex::new(
				r"(?:\d+) ([^,]+) bag"
			).unwrap();
		}

		let top_caps = TOP_RE.captures(&line).unwrap();
		let color = top_caps[1].to_string();

		for cap in CONTENTS_RE.captures_iter(&top_caps[2]) {
			let contents_color = cap[1].to_string();
			let v = result.entry(contents_color).or_insert(Vec::new());
			v.push(color.clone());
		}
	}

	result
}

#[test]
fn test_parse_rules() {
	let actual = parse_rules("light red bags contain 1 bright white bag, 2 muted yellow bags.
light blue bags contain 1 bright white bag.
bright white bags contain 1 shiny gold bag.");
	let mut expected = HashMap::new();
	expected.insert(
		"bright white".to_string(),
		vec!["light red".to_string(), "light blue".to_string()]
	);
	expected.insert(
		"muted yellow".to_string(),
		vec!["light red".to_string()]
	);
	expected.insert(
		"shiny gold".to_string(),
		vec!["bright white".to_string()]
	);
	assert_eq!(actual, expected);
}

fn can_contain_shiny_gold(
	rules: HashMap<String, Vec<String>>
) -> HashSet<String> {

	let mut result = HashSet::new();
	can_contain(&mut result, &rules, "shiny gold".to_string());
	result
}

fn can_contain(
	mut result: &mut HashSet<String>,
	rules: &HashMap<String, Vec<String>>,
	target: String
) {
	match rules.get(&target) {
		None => { /* A "root" color, contained in nothing. */ },
		Some(containers) => {
			for container in containers {
				result.insert(container.to_string());
				can_contain(&mut result, &rules, container.to_string());
			}
		}
	}
}

#[test]
fn test_can_contain_shiny_gold() {
	let rules = parse_rules("light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.");
	let mut expected: HashSet<String> = HashSet::new();
	expected.insert("bright white".to_string());
	expected.insert("muted yellow".to_string());
	expected.insert("dark orange".to_string());
	expected.insert("light red".to_string());

	assert_eq!(can_contain_shiny_gold(rules), expected);
}
