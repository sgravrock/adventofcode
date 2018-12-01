extern crate regex;
use regex::Regex;
#[cfg(test)]
extern crate timebomb;
use std::collections::BTreeSet;
use std::collections::VecDeque;


fn main() {
	let input = "The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, a strontium generator, a elerium generator, a elerium-compatible microchip, a dilithium generator, a dilithium-compatible microchip.
The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
The fourth floor contains nothing relevant.";
	let start = parse_input(input);
	println!("{:?}", shortest_path(start));
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
enum ThingType {
	Gen,
	Chip
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
struct ScienceThing {
	kind: ThingType,
	molecule: String
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
struct Move<'a> {
	from: &'a State,
	to_floor: usize,
	carrying: BTreeSet<ScienceThing>
}

impl<'a> Move<'a> {
	fn apply(&self) -> State {
		let mut i = 0;
		let floors = self.from.floors.iter()
			.map(|things| {
				let fi = i;
				i += 1;

				if fi == self.from.current_floor {
					things - &self.carrying
				} else if fi == self.to_floor {
					things.union(&self.carrying).cloned().collect()
				} else {
					things.clone()
				}
			})
			.collect();

		State { floors: floors, current_floor: self.to_floor }
	}
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
struct State {
	floors: Vec<BTreeSet<ScienceThing>>,
	current_floor: usize
}


fn parse_input(input: &str) -> State {
	let re = Regex::new("([a-z]+)(-compatible microchip| generator)").unwrap();

	let floors = input.split('\n')
		.map(|line| {
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
		.collect();

	State { floors: floors, current_floor: 0 }
}

#[test]
fn test_parse_input() {
	let actual = parse_input("The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
The fourth floor contains nothing relevant.");
	let floors: Vec<BTreeSet<ScienceThing>> = vec![
		[
			gen("thulium"), chip("thulium"), gen("plutonium"), gen("strontium")
		].iter().cloned().collect(),
		[
			chip("plutonium"), chip("strontium")
		].iter().cloned().collect(),
		BTreeSet::new()
	];
	let expected = State { floors: floors, current_floor: 0 };
	assert_eq!(expected, actual);
}

#[cfg(test)]
fn chip(molecule: &str) -> ScienceThing {
	ScienceThing { kind: ThingType::Chip, molecule: molecule.to_string() }
}

#[cfg(test)]
fn gen(molecule: &str) -> ScienceThing {
	ScienceThing { kind: ThingType::Gen, molecule: molecule.to_string() }
}


fn shortest_path(start: State) -> Option<usize> {
	let mut visited = BTreeSet::new();
	let mut queue = VecDeque::new();
	queue.push_back((0, start));

	while !queue.is_empty() {
		let (n_moves, s) = queue.pop_front().unwrap();

		if !visited.insert(s.clone()) {
			continue;
		}

		for m in possible_moves(&s).iter() {
			let next_state = m.apply();

			if is_valid(&next_state.floors) {
				if is_finished(&next_state.floors) {
					return Some(n_moves + 1);
				}

				queue.push_back((n_moves + 1, next_state));
			}
		}
	}

	None
}

#[test]
fn shortest_path_terminates() {
	// No solution, but an infinite series of non-solution moves possible
	let start = parse_input("x-compatible microchip
nothing
y generator");
	timebomb::timeout_ms(|| {
		shortest_path(start);
	}, 1000);
}

#[test]
fn shortest_path_not_found() {
	let start = parse_input("x-compatible microchip
nothing
y generator");
	assert_eq!(None, shortest_path(start));
}

#[test]
fn shortest_path_easy() {
	let start = parse_input("x generator, x-compatible microchip
nothing
nothing");
	assert_eq!(Some(2), shortest_path(start));
}


#[test]
fn shortest_path_harder() {
let start = parse_input("The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.");
	assert_eq!(Some(11), shortest_path(start));
}


fn is_valid(floors: &Vec<BTreeSet<ScienceThing>>) -> bool {
	floors.iter().all(is_valid_floor)
}

fn is_valid_floor(things: &BTreeSet<ScienceThing>) -> bool {
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
	let floors: Vec<BTreeSet<ScienceThing>> = vec![
		[
			chip("thulium"), chip("plutonium")
		].iter().cloned().collect(),
	];
	assert_eq!(true, is_valid(&floors));
}

#[test]
fn test_is_valid_no_chips() {
	let floors: Vec<BTreeSet<ScienceThing>> = vec![
		[
			gen("thulium"), gen("plutonium")
		].iter().cloned().collect(),
	];
	assert_eq!(true, is_valid(&floors));
}

#[test]
fn test_is_valid_cant_fry() {
	let floors: Vec<BTreeSet<ScienceThing>> = vec![
		[
			gen("thulium"), chip("strontium")
		].iter().cloned().collect(),
	];
	assert_eq!(false, is_valid(&floors));
}

#[test]
fn test_is_valid_gen_protects_chip() {
	let floors: Vec<BTreeSet<ScienceThing>> = vec![
		[
			gen("thulium"), chip("strontium"), gen("strontium")
		].iter().cloned().collect(),
	];
	assert_eq!(true, is_valid(&floors));
}

fn is_finished(floors: &Vec<BTreeSet<ScienceThing>>) -> bool {
	for i in 0..floors.len() - 1 {
		if !floors[i].is_empty() {
			return false;
		}
	}

	true
}

#[test]
fn test_is_finished() {
	let yes: Vec<BTreeSet<ScienceThing>> = vec![
		BTreeSet::new(),
		[gen("a"), chip("a")].iter().cloned().collect(),
	];
	let no: Vec<BTreeSet<ScienceThing>> = vec![
		[gen("a"), chip("a")].iter().cloned().collect(),
		BTreeSet::new(),
	];
	assert_eq!(true, is_finished(&yes));
	assert_eq!(false, is_finished(&no));
}

fn possible_moves<'a>(from: &'a State) -> BTreeSet<Move<'a>> {
	let mut result = BTreeSet::new();
	let on_from_floor = from.floors[from.current_floor]
		.iter().cloned().collect();
	let carry_sets = candidate_carry_sets(on_from_floor);

	for c in carry_sets {
		if from.current_floor > 0 {
			result.insert(Move {
				from: from,
				to_floor: from.current_floor - 1,
				carrying: c.iter().cloned().collect()
			});
		}

		if from.current_floor + 1 < from.floors.len() {
			result.insert(Move {
				from: from,
				to_floor: from.current_floor + 1,
				carrying: c.iter().cloned().collect()
			});
		}
	}

	result
}

#[test]
fn possible_moves_only_adjacent_floors() {
	let floors: Vec<BTreeSet<ScienceThing>> = vec![
		[ gen("a") ].iter().cloned().collect(),
		[ gen("b") ].iter().cloned().collect(),
		[ gen("c") ].iter().cloned().collect(),
		[ gen("d") ].iter().cloned().collect(),
	];
	let s0 = State { floors: floors.clone(), current_floor: 0 };
	let f0: Vec<usize> = possible_moves(&s0)
		.iter()
		.map(|m| m.to_floor)
		.collect();
	assert_eq!(vec![1], f0);
	let s1 = State { floors: floors.clone(), current_floor: 1 };
	let mut f1: Vec<usize> = possible_moves(&s1)
		.iter()
		.map(|m| m.to_floor)
		.collect();
	f1.sort();
	assert_eq!(vec![0, 2], f1);
	let s3 = State { floors: floors, current_floor: 3 };
	let f3: Vec<usize> = possible_moves(&s3)
		.iter()
		.map(|m| m.to_floor)
		.collect();
	assert_eq!(vec![2], f3);
}

#[test]
fn possible_moves_must_carry_1_to_2() {
	let floors: Vec<BTreeSet<ScienceThing>> = vec![
		[
			gen("a"), gen("b"), gen("c")
		].iter().cloned().collect(),
		[].iter().cloned().collect(),
	];
	let s = State { floors: floors, current_floor: 0 };
	let mut expected = BTreeSet::new();
	expected.insert(Move {
		from: &s,
		to_floor: 1,
		carrying: [gen("a")].iter().cloned().collect()
	});
	expected.insert(Move {
		from: &s,
		to_floor: 1,
		carrying: [gen("b")].iter().cloned().collect()
	});
	expected.insert(Move {
		from: &s,
		to_floor: 1,
		carrying: [gen("c")].iter().cloned().collect()
	});
	expected.insert(Move {
		from: &s,
		to_floor: 1,
		carrying: [gen("a"), gen("b")].iter().cloned().collect()
	});
	expected.insert(Move {
		from: &s,
		to_floor: 1,
		carrying: [gen("a"), gen("c")].iter().cloned().collect()
	});
	expected.insert(Move {
		from: &s,
		to_floor: 1,
		carrying: [gen("b"), gen("c")].iter().cloned().collect()
	});
	assert_eq!(expected, possible_moves(&s));
}

fn candidate_carry_sets(items: Vec<ScienceThing>) -> Vec<Vec<ScienceThing>> {
	let mut result = vec![];

	for i in 0..items.len() {
		result.push(vec![items[i].clone()]);

		for j in (i + 1)..items.len() {
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
