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
			ScienceThing { kind: ThingType::Gen, molecule: "thulium".to_string() },
			ScienceThing { kind: ThingType::Chip, molecule: "thulium".to_string() },
			ScienceThing { kind: ThingType::Gen, molecule: "plutonium".to_string() },
			ScienceThing { kind: ThingType::Gen, molecule: "strontium".to_string() },
		].iter().cloned().collect(),
		[
			ScienceThing { kind: ThingType::Chip, molecule: "plutonium".to_string() },
			ScienceThing { kind: ThingType::Chip, molecule: "strontium".to_string() },
		].iter().cloned().collect(),
		HashSet::new()
	];
	assert_eq!(expected, actual);
}

