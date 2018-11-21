use std::collections::HashSet;

fn main() {
	let components = Component::parse(
"32/31
2/2
0/43
45/15
33/24
20/20
14/42
2/35
50/27
2/17
5/45
3/14
26/1
33/38
29/6
50/32
9/48
36/34
33/50
37/35
12/12
26/13
19/4
5/5
14/46
17/29
45/43
5/0
18/18
41/22
50/3
4/4
17/1
40/7
19/0
33/7
22/48
9/14
50/43
26/29
19/33
46/31
3/16
29/46
16/0
34/17
31/7
5/27
7/4
49/49
14/21
50/9
14/44
29/29
13/38
31/11");
	println!("Puzzle solution is {}.", strongest_bridge(components));
}

fn strongest_bridge(components: Vec<Component>) -> u32 {
	generate_combinations(components)
		.iter()
		.map(bridge_strength)
		.max()
		.unwrap()
}

fn bridge_strength(bridge: &Vec<Component>) -> u32 {
	bridge.iter()
		.map(|c| c.strength())
		.sum()
}

#[test]
fn test_strongest_bridge() {
	let components = Component::parse(
"0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10");
	assert_eq!(strongest_bridge(components), 31);
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
struct Component {
	ports: (u32, u32)
}

impl Component {
	fn new(p0: u32, p1: u32) -> Component {
		Component { ports: (p0, p1) }
	}

	fn parse(input: &str) -> Vec<Component> {
		input.split('\n')
			.map(|line| {
				let ports = line.split('/')
					.map(|t| t.parse::<u32>().unwrap())
					.collect::<Vec<u32>>();
				Component::new(ports[0], ports[1])
			})
			.collect()
	}

	fn strength(&self) -> u32 {
		self.ports.0 + self.ports.1
	}
}

// TODO: This is very, very slow. Could using a generator make it faster?
fn generate_combinations(mut elements: Vec<Component>) -> HashSet<Vec<Component>> {
	let mut result = HashSet::new();

	permutohedron::heap_recursive(&mut elements, |permutation| {
		combinations_by_removal(permutation.to_vec(), &mut result);
	});

	result
}

fn combinations_by_removal(all_elements: Vec<Component>, mut dest: &mut HashSet<Vec<Component>>) {
	if is_valid(&all_elements) {
		dest.insert(all_elements.clone());
	}

	if all_elements.len() > 1 {
		for i in 0..all_elements.len() {
			let mut sub = all_elements.clone();
			sub.remove(i);

			if is_valid(&sub) {
				dest.insert(sub.clone());
			}

			combinations_by_removal(sub, &mut dest);
		}
	}
}

#[test]
fn test_generate_combinations() {
	let components = Component::parse(
"0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10");
	let mut expected = HashSet::new();
	expected.insert(Component::parse("0/1"));
	expected.insert(Component::parse("0/1\n10/1"));
	expected.insert(Component::parse("0/1\n10/1\n9/10"));
	expected.insert(Component::parse("0/2"));
	expected.insert(Component::parse("0/2\n2/3"));
	expected.insert(Component::parse("0/2\n2/3\n3/4"));
	expected.insert(Component::parse("0/2\n2/3\n3/5"));
	expected.insert(Component::parse("0/2\n2/2"));
	expected.insert(Component::parse("0/2\n2/2\n2/3"));
	expected.insert(Component::parse("0/2\n2/2\n2/3\n3/4"));
	expected.insert(Component::parse("0/2\n2/2\n2/3\n3/5"));

	let actual = generate_combinations(components);
	assert_eq!(actual, expected);
}

fn is_valid(sequence: &Vec<Component>) -> bool {
	let mut next_port = 0;

	for c in sequence {
		if c.ports.0 == next_port {
			next_port = c.ports.1;
		} else if c.ports.1 == next_port {
			next_port = c.ports.0;
		} else {
			return false;
		}
	}

	true
}
