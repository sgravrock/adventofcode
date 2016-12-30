extern crate regex;
use regex::Regex;
use std::collections::HashSet;
use std::collections::BTreeSet;
use std::hash::Hash;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone)]
enum ThingType {
	Gen,
	Chip
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone)]
struct ScienceThing {
	kind: ThingType,
	molecule: String
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct Move {
	from_floor: usize,
	to_floor: usize,
	carrying: BTreeSet<ScienceThing>
}


fn main() {
	let input = "The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
The fourth floor contains nothing relevant.";
	let floors = parse_input(input);
}

fn parse_input(input: &str) -> Vec<HashSet<ScienceThing>> {
	let re = Regex::new("([a-z]+)(-compatible microchip| generator)").unwrap();

	input.split('\n')
		.map(|line| {
			println!("checking {}", line);
			re.captures_iter(line)
				.map(|cap| {
					let m = cap.at(1).unwrap().to_string();
					let kind = match cap.at(2).unwrap() {
						"-compatible microchip" => ThingType::Chip,
						_ => ThingType::Gen
					};
					ScienceThing { kind: kind, molecule: m }
				})
				.collect()
		})
		.collect()
}

#[test]
fn test_parse_input() {
	let actual = parse_input("The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
The fourth floor contains nothing relevant.");
	let expected: Vec<HashSet<ScienceThing>> = vec![
		[
			gen("thulium"), chip("thulium"), gen("plutonium"), gen("strontium")
		].iter().cloned().collect(),
		[
			chip("plutonium"), chip("strontium")
		].iter().cloned().collect(),
		HashSet::new()
	];
	assert_eq!(expected, actual);
}

fn chip(molecule: &str) -> ScienceThing {
	ScienceThing { kind: ThingType::Chip, molecule: molecule.to_string() }
}

fn gen(molecule: &str) -> ScienceThing {
	ScienceThing { kind: ThingType::Gen, molecule: molecule.to_string() }
}

fn is_valid(floors: &Vec<HashSet<ScienceThing>>) -> bool {
	floors.iter().all(is_valid_floor)
}

fn is_valid_floor(things: &HashSet<ScienceThing>) -> bool {
	let (chips, gens): (Vec<&ScienceThing>, Vec<&ScienceThing>) = 
		things.iter().partition(|thing| {
			match thing.kind {
				ThingType::Chip => true,
				_ => false
			}
		});
	let unprocteted_chips = chips.iter().any(|c| {
		!gens.iter().any(|g| g.molecule == c.molecule)
	});
	gens.len() == 0 || !unprocteted_chips
}

#[test]
fn test_is_valid_no_gens() {
	let floors: Vec<HashSet<ScienceThing>> = vec![
		[
			chip("thulium"), chip("plutonium")
		].iter().cloned().collect(),
	];
	assert_eq!(true, is_valid(&floors));
}

#[test]
fn test_is_valid_no_chips() {
	let floors: Vec<HashSet<ScienceThing>> = vec![
		[
			gen("thulium"), gen("plutonium")
		].iter().cloned().collect(),
	];
	assert_eq!(true, is_valid(&floors));
}

#[test]
fn test_is_valid_cant_fry() {
	let floors: Vec<HashSet<ScienceThing>> = vec![
		[
			gen("thulium"), chip("strontium")
		].iter().cloned().collect(),
	];
	assert_eq!(false, is_valid(&floors));
}

#[test]
fn test_is_valid_gen_protects_chip() {
	let floors: Vec<HashSet<ScienceThing>> = vec![
		[
			gen("thulium"), chip("strontium"), gen("strontium")
		].iter().cloned().collect(),
	];
	assert_eq!(true, is_valid(&floors));
}

fn possible_moves(from: usize, floors: &Vec<HashSet<ScienceThing>>) -> HashSet<Move> {
	let mut result = HashSet::new();
	let carry_sets = candidate_carry_sets(floors[from].iter().cloned().collect());

	for c in carry_sets {
		if from > 0 {
			result.insert(Move {
				from_floor: from,
				to_floor: from - 1,
				carrying: c.iter().cloned().collect()
			});
		}

		if from + 1 < floors.len() {
			result.insert(Move {
				from_floor: from,
				to_floor: from + 1,
				carrying: c.iter().cloned().collect()
			});
		}
	}

	result
}

#[test]
fn possible_moves_only_adjacent_floors() {
	let floors: Vec<HashSet<ScienceThing>> = vec![
		[ gen("a") ].iter().cloned().collect(),
		[ gen("b") ].iter().cloned().collect(),
		[ gen("c") ].iter().cloned().collect(),
		[ gen("d") ].iter().cloned().collect(),
	];
	let f0: Vec<usize> = possible_moves(0, &floors)
		.iter()
		.map(|m| m.to_floor)
		.collect();
	assert_eq!(vec![1], f0);
	let mut f1: Vec<usize> = possible_moves(1, &floors)
		.iter()
		.map(|m| m.to_floor)
		.collect();
	f1.sort();
	assert_eq!(vec![0, 2], f1);
	let f3: Vec<usize> = possible_moves(3, &floors)
		.iter()
		.map(|m| m.to_floor)
		.collect();
	assert_eq!(vec![2], f3);
}

#[test]
fn possible_moves_must_carry_1_to_2() {
	let floors: Vec<HashSet<ScienceThing>> = vec![
		[
			gen("a"), gen("b"), gen("c")
		].iter().cloned().collect(),
		[].iter().cloned().collect(),
	];
	let mut expected: HashSet<Move> = HashSet::new();
	expected.insert(Move {
		from_floor: 0,
		to_floor: 1,
		carrying: [gen("a")].iter().cloned().collect()
	});
	expected.insert(Move {
		from_floor: 0,
		to_floor: 1,
		carrying: [gen("b")].iter().cloned().collect()
	});
	expected.insert(Move {
		from_floor: 0,
		to_floor: 1,
		carrying: [gen("c")].iter().cloned().collect()
	});
	expected.insert(Move {
		from_floor: 0,
		to_floor: 1,
		carrying: [gen("a"), gen("b")].iter().cloned().collect()
	});
	expected.insert(Move {
		from_floor: 0,
		to_floor: 1,
		carrying: [gen("a"), gen("c")].iter().cloned().collect()
	});
	expected.insert(Move {
		from_floor: 0,
		to_floor: 1,
		carrying: [gen("b"), gen("c")].iter().cloned().collect()
	});
	assert_eq!(expected, possible_moves(0, &floors));
}

fn candidate_carry_sets(items: Vec<ScienceThing>) -> Vec<Vec<ScienceThing>> {
	let mut result = vec![];

	for i in (0..items.len()) {
		result.push(vec![items[i].clone()]);

		for j in ((i + 1)..items.len()) {
			result.push(vec![items[i].clone(), items[j].clone()]);
		}
	}

	result
}

#[test]
fn test_candidate_carry_sets() {
	let things = vec![gen("a") ,gen("b"), gen("c")];
	let expected: Vec<Vec<ScienceThing>> = vec![
		vec![gen("a")],
		vec![gen("a"), gen("b")],
		vec![gen("a"), gen("c")],
		vec![gen("b")],
		vec![gen("b"), gen("c")],
		vec![gen("c")],
	];
	assert_eq!(expected, candidate_carry_sets(things));
}
