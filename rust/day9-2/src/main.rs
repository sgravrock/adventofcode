use std::io::{self, Read};


fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	println!("{}", decompressed_length(&input));
}

fn decompressed_length(input: &str) -> usize {
	let mut result = 0 as usize;
	let mut scanner = Scanner::new(input);

	while !scanner.is_empty() {
		result += match next_repeatable(&mut scanner) {
			Some(rep) => rep,
			None => scanner.scan_to('(').unwrap().len()
		};
	}

	result
}

#[test]
fn test_decompress() {
	assert_eq!(decompressed_length("ADVENT"), 6);
	assert_eq!(decompressed_length("(3x3)XYZ"), 9);
	assert_eq!(decompressed_length("X(8x2)(3x3)ABCY"), 20);
	assert_eq!(decompressed_length("(27x12)(20x12)(13x14)(7x10)(1x12)A"), 241920);
	assert_eq!(decompressed_length("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"), 445);
}

#[derive(PartialEq, Debug)]
struct Repetition {
	s: String,
	times: u32, 
}

fn next_repeatable(scanner: &mut Scanner) -> Option<usize> {
	match scan_repetition(scanner) {
		Some(rep) => {
			let n = decompressed_length(&rep.s);
			Some(n * rep.times as usize)
		},
		None => None,
	}
}

fn scan_repetition(scanner: &mut Scanner) -> Option<Repetition> {
	// Do one of three things: return None without consuming anything,
	// return Some, or panic.
	if !scanner.consume('(') {
		return None;
	}

	let len = scanner.scan_int().unwrap();
	scanner.consume('x');
	let ntimes = scanner.scan_int().unwrap();
	scanner.consume(')');
	let s = scanner.scan_next_n(len);
	Some(Repetition {s: s, times: ntimes })
}

#[test]
fn test_scan_repetition() {
	let mut scanner = Scanner::new("(2x3)abc");
	assert_eq!(scan_repetition(&mut scanner),
		Some(Repetition { s: "ab".to_string(), times: 3 }));
	assert_eq!(scan_repetition(&mut scanner), None);
	assert_eq!(scanner.scan_to_end(), "c".to_string());
}

struct Scanner<'a> {
	chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl <'a> Scanner<'a> {
	pub fn new(input: &str) -> Scanner {
		Scanner { 
			chars: input.chars().peekable()
		}
	}

	pub fn is_empty(&mut self) -> bool {
		match self.chars.peek() {
			Some(_) => false,
			None => true
		}
	}

	pub fn scan_to_end(&mut self) -> String {
		let mut v: Vec<char> = vec![];

		loop {
			match self.chars.next() {
				Some(c) => v.push(c),
				None => break
			}
		}

		v.iter().map(|r| *r).collect()
	}

	pub fn scan_to(&mut self, delim: char) -> Option<String> {
		let mut result = String::new();

		while !self.next_is_delim_or_end(delim) {
			result.push(self.chars.next().unwrap());
		}

		match result.len() {
			0 => None,
			_ => Some(result)
		}
	}

	pub fn scan_next_n(&mut self, n: u32) -> String {
		let mut s = String::new();

		for _ in 0..n {
			match self.chars.next() {
				Some(c) => s.push(c),
				None => break
			}
		}

		s
	}

	pub fn scan_int(&mut self) -> Option<u32> {
		let mut s = String::new();

		while self.next_is_numeric() {
			s.push(self.chars.next().unwrap());
		}

		match s.len() {
			0 => None,
			_ => Some(s.parse::<u32>().unwrap())
		}
	}

	pub fn consume(&mut self, expected: char) -> bool {
		if self.next_is(expected) {
			self.chars.next();
			true
		} else {
			false
		}
	}

	fn next_is(&mut self, expected: char) -> bool {
		self.chars.peek() == Some(&expected)
	}

	fn next_is_numeric(&mut self) -> bool {
		self.chars.peek().map(|c| c.is_numeric()).unwrap_or(false)
	}

	fn next_is_delim_or_end(&mut self, delim: char) -> bool {
		match self.chars.peek() {
			Some(c) => *c == delim,
			None => true
		}
	}
}

#[test]
fn test_scanner_scan_to_end() {
	assert_eq!(Scanner::new("abc").scan_to_end(), "abc");
	let mut s = Scanner::new("abc");
	s.scan_to('b');
	assert_eq!(s.scan_to_end(), "bc");
	assert_eq!(s.scan_to_end(), "");
}

#[test]
fn test_scanner_scan_to() {
	let mut scanner = Scanner::new("12345");
	assert_eq!(scanner.scan_to('3'), Some("12".to_string()));
	assert_eq!(scanner.scan_to('1'), Some("345".to_string()));
	assert_eq!(scanner.scan_to('1'), None);
}

#[test]
fn test_scan_next_n() {
	let mut scanner = Scanner::new("abc");
	assert_eq!(scanner.scan_next_n(2), "ab".to_string());
	assert_eq!(scanner.scan_next_n(2), "c".to_string());
}

#[test]
fn test_scan_int() {
	let mut scanner = Scanner::new("123x");
	assert_eq!(scanner.scan_int(), Some(123));
	assert_eq!(scanner.scan_int(), None);
	assert_eq!(scanner.scan_to_end(), "x");
}

#[test]
fn test_scanner_consume() {
	let mut scanner = Scanner::new("1");
	assert_eq!(scanner.consume('2'), false);
	assert_eq!(scanner.consume('1'), true);
	assert_eq!(scanner.consume('1'), false);
}
