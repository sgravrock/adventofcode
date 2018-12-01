use std::collections::VecDeque;
use std::hash::Hash;
#[cfg(test)]
use std::collections::HashSet;


fn main() {
	let visited = breadth_first_traverse((1, 1), 51,
		|r| next_coords(1364, r));
   println!("{}", visited.len());
}

fn next_coords(key: i32, from: (i32, i32)) -> Vec<(i32, i32)> {
	let candidates = vec![
		(from.0 - 1, from.1),
		(from.0 + 1, from.1),
		(from.0, from.1 - 1),
		(from.0, from.1 + 1),
	];

	candidates.iter()
		.filter(|c| c.0 >= 0 && c.1 >= 0 && is_open(key, &c))
		.cloned()
		.collect()
}

#[test]
fn test_next_coords() {
	let expected1: HashSet<(i32,i32)> = [(3, 1), (4, 2), (3, 3), (2, 2)]
		.iter().cloned().collect();
	assert_eq!(expected1,
		next_coords(10, (3, 2)).iter().cloned().collect());

	let expected2: HashSet<(i32,i32)> = [(0, 1), (1, 2)]
		.iter().cloned().collect();
	assert_eq!(expected2, next_coords(10, (1, 1)).iter().cloned().collect());

	let expected3 = vec![(0, 1)];
	assert_eq!(expected3, next_coords(10, (0, 0)));
}

fn is_open(key: i32, coords: &(i32, i32)) -> bool {
	let n = coords.0*coords.0 + 3*coords.0 + 2*coords.0*coords.1 +
		coords.1 + coords.1*coords.1 + key;
	bits_set(n as u32) % 2 == 0
}

#[test]
fn test_is_open() {
	assert!(is_open(10, &(3, 1)));
}

fn bits_set(x: u32) -> u32 {
   let mut n = 0;
    
   for i in 0..32 {
       if x & (1 << i) != 0 {
           n += 1;
       }
   }

	n
}

fn breadth_first_traverse<T, F>(src: T, max_depth: i32, find_next: F) 
		-> Vec<T>
		where T: Copy + Hash + Eq,
		F: Fn(T) -> Vec<T> {

	let mut queue = VecDeque::new();
	queue.push_back((1, src));
	let mut visited: Vec<T> = vec![];

	while !queue.is_empty() {
		let (depth, pos) = queue.pop_front().unwrap();

		if visited.contains(&pos) {
			continue;
		}

		visited.push(pos);

		if depth < max_depth {
			for p in find_next(pos) {
				queue.push_back((depth + 1, p));
			}
		}
	}

	visited
}

#[test]
fn test_breadth_first_traverse_order() {
	let find_next = |n| {
		match n {
			0 => vec![1, 2],
			1 => vec![3],
			2 => vec![42],
			3 => vec![43],
			_ => vec![]
		}
	};
	
	let visited = breadth_first_traverse(0, 4, find_next);
	assert_eq!(vec![0, 1, 2, 3, 42, 43], visited);
}

#[test]
fn test_breadth_first_traverse_obeys_limit() {
	let find_next = |n| {
		match n {
			0 => vec![1, 2],
			1 => vec![3],
			2 => vec![4],
			4 => vec![5],
			5 => vec![2],
			_ => vec![]
		}
	};
	
	let visited = breadth_first_traverse(0, 3, find_next);
	assert_eq!(vec![0, 1, 2, 3, 4], visited);
}

#[test]
fn test_breadth_first_traverse_no_repeat() {
	let find_next = |n| {
		match n {
			0 => vec![1],
			1 => vec![0],
			_ => vec![]
		}
	};
	
	let visited = breadth_first_traverse(0, 10, find_next);
	assert_eq!(vec![0, 1], visited);
}
