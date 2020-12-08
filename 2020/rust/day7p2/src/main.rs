#[macro_use] extern crate lazy_static;
extern crate regex;
use regex::Regex;
use std::collections::HashMap;
mod input;

fn main() {
	println!("{}", num_contained_in_shiny_gold(parse_rules(input::puzzle_input())));
	// 1250
}

fn parse_rules(input: &str) -> HashMap<String, Vec<(u32, String)>> {
	input.lines()
		.map(|line| {
			lazy_static! {
				static ref TOP_RE: Regex = Regex::new(
					r"(.+) bags contain (.+)\.$"
				).unwrap();
				static ref CONTENTS_RE: Regex = Regex::new(
					r"(\d+) ([^,]+) bag"
				).unwrap();
			}
	
			let top_caps = TOP_RE.captures(&line).unwrap();
			let color = top_caps[1].to_string();
			let contents: Vec<(u32, String)> = CONTENTS_RE.captures_iter(&top_caps[2])
				.map(|cap| {
					let n: u32 = cap[1].parse().unwrap();
					let contents_color = cap[2].to_string();
					(n, contents_color)
				}).collect();
				(color, contents)
		})
		.collect()
}

#[test]
fn test_parse_rules() {
	let actual = parse_rules("light red bags contain 1 bright white bag, 2 muted yellow bags.
light blue bags contain 1 bright white bag.
bright white bags contain 1 shiny gold bag.");
	let mut expected = HashMap::new();
	expected.insert(
		"light red".to_string(),
		vec![
			(1, "bright white".to_string()),
			(2, "muted yellow".to_string())
		]
	);
	expected.insert(
		"light blue".to_string(),
		vec![(1, "bright white".to_string())]
	);
	expected.insert(
		"bright white".to_string(),
		vec![(1, "shiny gold".to_string())]
	);
	assert_eq!(actual, expected);
}

fn num_contained_in_shiny_gold(
	rules: HashMap<String, Vec<(u32, String)>>
) -> u32 {
	num_contained_in(&rules, "shiny gold")
}

fn num_contained_in(
	rules: &HashMap<String, Vec<(u32, String)>>,
	container: &str
) -> u32 {
	let n = rules[container].iter()
		.map(|(qty, color)| qty * (1 + num_contained_in(&rules, color)))
		.sum();
	n
}

#[test]
fn test_num_contained_in_shiny_gold_small() {
	let rules = parse_rules("light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.");
	
	assert_eq!(num_contained_in_shiny_gold(rules), 32);
}

#[test]
fn test_num_contained_in_shiny_gold_bigger() {
	let rules = parse_rules("shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.");
	assert_eq!(num_contained_in_shiny_gold(rules), 126);
}
