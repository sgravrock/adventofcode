fn main() {
    println!("{}", find_winning_elf(3001330));
}

fn find_winning_elf(num_elves: usize) -> usize {
	let mut ring = ElfRing::new(num_elves);
	let mut i = 0;

	while ring.len() > 1 {
		i = ring.remove_across_from(i);
	}

	ring.elf_at(0)
}

#[test]
fn test_find_winning_elf() {
	assert_eq!(1, find_winning_elf(4));
	assert_eq!(2, find_winning_elf(5));
}


struct ElfRing {
	elves: Vec<usize>
}

impl ElfRing {
	fn new(n: usize) -> ElfRing {
		ElfRing { elves: (1..(n + 1)).collect() }
	}

	fn len(&self) -> usize {
		self.elves.len()
	}

	fn elf_at(&self, i: usize) -> usize {
		self.elves[i]
	}
		
	fn remove_across_from(&mut self, i: usize) -> usize {
		let to_remove = (i + (self.elves.len() / 2)) % self.elves.len();
		self.elves.remove(to_remove);

		if i < to_remove {
			(i + 1) % self.elves.len()
		} else {
			i % self.elves.len()
		}
	}
}

#[test]
fn test_remove_across_from() {
	let mut r1 = ElfRing::new(4);
	assert_eq!(2, r1.remove_across_from(1));
	assert_eq!(vec![1, 2, 3], r1.elves);

	let mut r2 = ElfRing::new(5);
	assert_eq!(1, r2.remove_across_from(0));
	assert_eq!(vec![1, 2, 4, 5], r2.elves);
	assert_eq!(2, r2.remove_across_from(1));
	assert_eq!(vec![1, 2, 4], r2.elves);
	//assert_eq!(1, r2.remove_across_from(2));
	assert_eq!(0, r2.remove_across_from(2));
	assert_eq!(vec![2, 4], r2.elves);
	assert_eq!(0, r2.remove_across_from(0));
	assert_eq!(vec![2], r2.elves);
}
