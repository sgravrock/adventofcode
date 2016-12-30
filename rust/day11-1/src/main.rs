extern crate regex;
use regex::Regex;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
enum ThingType {
	Gen,
	Chip
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct ScienceThing {
	kind: ThingType,
	molecule: String
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
