mod input;
use std::fmt;
use std::collections::HashMap;

#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::iterators::Pair;

fn main() {
	let result = ore_required(parse_input(input::puzzle_input()), 1);
	println!("Need {} ore to produce 1 FUEL", result);
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

fn ore_required(mut reactions: Vec<Reaction>, num_fuel: i32) -> i32 {
	let mut needs: HashMap<String, i32> = HashMap::new();
	needs.insert("FUEL".to_string(), num_fuel);

	while reactions.len() > 0 {
		// Find a reaction whose output isn't an input to anything we haven't
		// yet processed. This ensures that we add up all the needed quantities
		// and process it once. Otherwise we might round off multiple times.
		let root = reactions.remove(find_first_root(&reactions));
		let output_name = root.output.0;
		let output_qty_needed = needs.remove(&output_name)
			.unwrap_or_else(|| panic!("Didn't find {} in needs"));

		// Round up to the nearest integer multiple of the reaction's output qty
		let reactions_needed =
			(output_qty_needed as f32 / root.output.1 as f32).ceil() as i32;

		// Figure out how many of each input we need and add them to NEEDS.
		for input in root.inputs {
			let input_qty_needed = needs.entry(input.0).or_insert(0);
			*input_qty_needed += input.1 * reactions_needed;
		}
	}

	assert_eq!(needs.len(), 1);
	*needs.get("ORE").unwrap_or_else(|| panic!("Didn't produce an ORE"))

}

// Finds a reaction that is not an input to any othe reactions.
fn find_first_root(reactions: &Vec<Reaction>) -> usize {
	for i in 0..reactions.len() {
		if !is_dependency(&reactions[i].output.0, &reactions) {
			return i;
		}
	}

	panic!("Couldn't find a root in {:?}", reactions);
}

fn is_dependency(output_name: &str, reactions: &Vec<Reaction>) -> bool {
	any(reactions.iter(), |r| {
		any(r.inputs.iter(), |input| input.0 == output_name)
	})
}

fn any<I, E, P>(iter: I, predicate: P) -> bool
		where I: Iterator<Item=E>,
		P: FnMut(&E) -> bool {
	iter.filter(predicate).next().is_some()
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
	assert_eq!(ore_required(reactions, 1), 165);
}

#[test]
fn test_ore_required_exact() {
	let reactions = parse_input("9 ORE => 1 FUEL");
	assert_eq!(ore_required(reactions, 1), 9);
}

#[test]
fn test_ore_required_multiple_fuel() {
	let reactions = parse_input("9 ORE => 1 FUEL");
	assert_eq!(ore_required(reactions, 10), 90);
}

#[test]
fn test_ore_required_can_multiply() {
	let reactions = parse_input("
		9 ORE => 2 A
		5 A => 1 FUEL
	");
	assert_eq!(ore_required(reactions, 1), 27);
}

#[test]
fn test_ore_required_for_cannot_divide() {
	let reactions = parse_input("
		9 ORE => 2 A
		1 A => 1 FUEL
	");
	assert_eq!(ore_required(reactions, 1), 9);
}


#[test]
fn test_ore_required_multiple_inputs() {
	let reactions = parse_input("
		3 ORE => 2 X
		3 X, 2 ORE => 1 FUEL
	");
	assert_eq!(ore_required(reactions, 1), 8);
}

#[test]
fn test_ore_required_merges_reactions_for_same_product() {
	let reactions = parse_input("
		3 ORE => 4 A
		3 A => 1 B
		1 A, 1 B => 1 FUEL
	");
	assert_eq!(ore_required(reactions, 1), 3);
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
