mod input;
use std::fmt;

#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::iterators::Pair;

fn main() {
}

#[derive(Parser)]
#[grammar = "input.pest"]
struct InputParser;

type Component = (String, i32);

#[derive(PartialEq)]
struct Reaction {
	inputs: Vec<Component>,
	output: Component
}

impl fmt::Debug for Reaction {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for i in 0..self.inputs.len() {
			write!(f, "{} {}", self.inputs[i].1, self.inputs[i].0)?;

			if i < self.inputs.len() - 1 {
				write!(f, ", ")?;
			}
		}

		write!(f, " => {} {}", self.output.1, self.output.0)
	}
}

fn parse_input(input: &str) -> Vec<Reaction> {
	let mut pairs = InputParser::parse(Rule::recipe, input).unwrap();
	let root = pairs.next().unwrap();
	assert!(pairs.next().is_none());

	root.into_inner()
		.filter_map(|pair| {
			match pair.as_rule() {
				Rule::reaction => Some(untree_reaction(pair)),
				Rule::EOI => None,
				_ => panic!("Expected a reaction or EOI but got {:?}",
					pair.as_rule())
			}
		})
		.collect()
}

fn untree_reaction<'a>(pair: Pair<'a, Rule>) -> Reaction {
	let mut children = pair.into_inner();
	let inputs = untree_input_list(children.next().unwrap());
	let output = untree_component(children.next().unwrap());

	if children.next().is_some() {
		panic!("Didn't fully consume children");
	}

	Reaction { inputs, output }
}

fn untree_input_list<'a>(pair: Pair<'a, Rule>) -> Vec<Component> {
	match pair.as_rule() {
		Rule::input_list => {
			pair.into_inner()
				.map(untree_component)
				.collect()
		},
		_ => panic!("Expected an input_list but got a {:?}", pair.as_rule())
	}
}

fn untree_component<'a>(pair: Pair<'a, Rule>) -> Component {
	match pair.as_rule() {
		Rule::component => {
			let mut inner_rules = pair.into_inner();
			let qty = inner_rules
				.next().unwrap()
				.as_str()
				.parse::<i32>().unwrap();
			let name = inner_rules
				.next().unwrap()
				.as_str().to_string();
			(name, qty)
		},
		_ => panic!("Expected a component but got a {:?}", pair.as_rule())
	}
}


#[test]
fn test_parse_input() {
	let input = "
		7 ORE => 5 C
		2 AB, 3 BC, 4 CA => 1 FUEL
	";
	let expected = vec![
		Reaction {
			inputs: vec![
				(String::from("ORE"), 7)
			],
			output: (String::from("C"), 5)
		},
		Reaction {
			inputs: vec![
				(String::from("AB"), 2),
				(String::from("BC"), 3),
				(String::from("CA"), 4)
			],
			output: (String::from("FUEL"), 1)
		}
	];

	assert_eq!(parse_input(input), expected);
}
