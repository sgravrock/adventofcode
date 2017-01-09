fn main() {
    println!("{}", find_winning_elf(3001330));
}

fn find_winning_elf(num_elves: usize) -> usize {
	let mut ring = ElfRing::new(num_elves);
	let mut i = 0;

	while ring.len() > 1 {
		i = ring.remove_next(i);
	}

	ring.elf_at(0)
}

#[test]
fn test_find_winning_elf() {
	assert_eq!(1, find_winning_elf(4));
	assert_eq!(3, find_winning_elf(5));
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
		
	fn remove_next(&mut self, i: usize) -> usize {
		if i == self.len() - 1 {
			self.elves.remove(0);
		} else {
			self.elves.remove(i + 1);
		}

		if i + 1 >= self.len() {
			0
		} else {
			i + 1
		}
	}
}

#[test]
fn test_remove_next() {
	let mut r = ElfRing::new(4);
	assert_eq!(0, r.remove_next(2));
	assert_eq!(vec![1, 2, 3], r.elves);
	assert_eq!(1, r.remove_next(0));
	assert_eq!(vec![1, 3], r.elves);
	assert_eq!(0, r.remove_next(1));
	assert_eq!(vec![3], r.elves);
}
