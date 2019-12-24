-   Consume 64 ORE to produce 24 B.
/   Consume 64 ORE to produce 23 B.

-   Consume 56 ORE to produce 40 C.
/   Consume 63 ORE to produce 37 C.


#![feature(vec_remove_item)]
mod input;
use std::fmt;

#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::iterators::Pair;

fn main() {
}

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

fn ore_required(reactions: &Vec<Reaction>) -> i32 {
	ore_required_for(&reactions, "FUEL", 1)
}

fn ore_required_for(reactions: &Vec<Reaction>, product_name: &str, qty_needed: i32) -> i32 {
	if product_name == "ORE" {
		return qty_needed;
	}

	let product = reactions.iter()
		.filter(|r| r.output.0 == product_name)
		.next()
		.unwrap_or_else(|| panic!("Couldn't find reaction for {}", product_name));
	let output_unit_qty = product.output.1;
	let times_reaction_run = 
		(qty_needed as f32 / output_unit_qty as f32).ceil() as i32;

	let mut input_qtys_consumed: Vec<i32> = Vec::new();

	let result = product.inputs.iter()
		.map(|input| {
			let input_unit_qty = input.1;
			let input_reactions_needed =
				(qty_needed as f32 / (input_unit_qty * times_reaction_run) as f32)
					.ceil() as i32;
			input_qtys_consumed.push(input_reactions_needed * input_unit_qty * times_reaction_run);
			ore_required_for(
				&reactions,
				&input.0,
				input_reactions_needed * input_unit_qty * times_reaction_run
			)
		})
		.sum();

	print!("Consume ");

	for i in 0..product.inputs.len() {
		print!("{} {}, ", input_qtys_consumed[i], product.inputs[i].0);
	}

	println!(" to produce {} {}", qty_needed, product_name);

	result
}


#[derive(Parser)]
#[grammar = "input.pest"]
struct InputParser;

fn parse_input(input: &str) -> Vec<Reaction> {
	let mut pairs = match InputParser::parse(Rule::recipe, input) {
			Ok(p) => p,
			Err(e) => panic!("Parse error:\n{}", e)
	};
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
fn test_ore_required_example_1() {
	let reactions = parse_input("
		9 ORE => 2 A
		8 ORE => 3 B
		7 ORE => 5 C
		3 A, 4 B => 1 AB
		5 B, 7 C => 1 BC
		4 C, 1 A => 1 CA
		2 AB, 3 BC, 4 CA => 1 FUEL
	");
	assert_eq!(ore_required(&reactions), 165);
}

#[test]
fn test_ore_required_for_exact() {
	let reactions = parse_input("9 ORE => 1 FUEL");
	assert_eq!(ore_required_for(&reactions, "FUEL", 1), 9);
}

#[test]
fn test_ore_required_for_can_multiply() {
	let reactions = parse_input("9 ORE => 2 FUEL");
	assert_eq!(ore_required_for(&reactions, "FUEL", 5), 27);
}

#[test]
fn test_ore_required_for_cannot_divide() {
	let reactions = parse_input("9 ORE => 2 FUEL");
	assert_eq!(ore_required_for(&reactions, "FUEL", 1), 9);
}

#[test]
fn test_ore_required_recursive() {
	let reactions = parse_input("
		3 ORE => 2 X
		3 X => 1 FUEL
	");
	assert_eq!(ore_required_for(&reactions, "FUEL", 2), 9);
}

#[test]
fn test_ore_required_multiple_inputs() {
	let reactions = parse_input("
		3 ORE => 2 X
		3 X, 2 ORE => 1 FUEL
	");

	assert_eq!(ore_required_for(&reactions, "FUEL", 2), 13);
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
