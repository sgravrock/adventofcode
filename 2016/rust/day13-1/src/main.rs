use std::collections::VecDeque;
use std::collections::HashSet;
use std::hash::Hash;
#[cfg(test)]
extern crate timebomb;


fn main() {
    println!("{:?}", shortest_path_to_coords(1364,	(31,39)));
}

fn shortest_path_to_coords(key: i32, room: (i32, i32)) -> Option<i32> {
	shortest_path_length((1,1), room, |r| next_coords(key, r))
}

#[test]
fn test_shortest_path_to_coords() {
	assert_eq!(Some(11), shortest_path_to_coords(10, (7,4)));
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

fn shortest_path_length<T, F>(src: T, dest: T, find_next: F) -> Option<i32>
		where T: Copy + Eq + Hash,
		F: Fn(T) -> Vec<T> {

	let find_next_with_len = |pos: (i32, T)| {
		find_next(pos.1).iter()
			.map(|n| (pos.0 + 1, *n))
			.collect()
	};
	let is_dest_with_len = |pos: (i32, T)| pos.1 == dest;
	let r = bfs((0, src), is_dest_with_len, find_next_with_len);

	match r {
		Some((len, _)) => Some(len),
		None => None
	}
}

#[test]
fn test_shortest_path_length() {
	let nexts = |n| {
		match n {
			0 => vec![1, 2],
			1 => vec![3],
			2 => vec![42],
			3 => vec![43],
			43 => vec![42],
			_ => vec![]
		}
	};
	let r = shortest_path_length(0, 42, nexts);
	assert_eq!(Some(2), r);
}

fn bfs<T, F, P>(src: T, is_dest: P, find_next: F) -> Option<T>
		where T: Copy + Hash + Eq,
		P: Fn(T) -> bool,
		F: Fn(T) -> Vec<T> {

	let mut queue = VecDeque::new();
	queue.push_back(src);
	let mut visited = HashSet::new();

	while !queue.is_empty() {
		let pos = queue.pop_front().unwrap();

		if !visited.insert(pos) {
			continue;
		}

		if is_dest(pos) {
			return Some(pos);
		}

		for p in find_next(pos) {
			queue.push_back(p);
		}
	}

	None
}

#[test]
fn test_bfs_terminates() {
	let nexts = |n| {
		if n == 1 {
			vec![0]
		} else {
			vec![1]
		}
	};
	timebomb::timeout_ms(|| {
		let r = bfs(1, |n| n == 2, nexts);
		assert_eq!(None, r);
	}, 1000);
}

#[test]
fn test_bfs() {
	let nexts = |n| {
		match n {
			0 => vec![1, 2],
			1 => vec![3],
			2 => vec![42],
			3 => vec![43],
			_ => vec![]
		}
	};
	let r = bfs(0, |n| n > 40, nexts);
	assert_eq!(Some(42), r);
}
