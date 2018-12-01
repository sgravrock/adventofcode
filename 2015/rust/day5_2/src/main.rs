use std::io::{self, Read};

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	let words = input.lines();
	let nice = words.filter(|w| { is_nice(w) });
	println!("{}", nice.count());
}

fn is_nice(s: &str) -> bool {
	let chars: Vec<char> = s.chars().collect();
	has_doubles(&chars) && has_repeat(&chars)
}

fn has_doubles(chars: &Vec<char>) -> bool {

	for i in 0..chars.len() - 3 {
		for j in i + 2..chars.len() - 1 {
			if chars[j] == chars[i] && chars[j + 1] == chars[i + 1] {
				return true;
			}
		}
	}

	false
}

fn has_repeat(chars: &Vec<char>) -> bool {
	for i in 0..chars.len() - 2 {
		if chars[i + 2] == chars[i] {
			return true;
		}
	}

	false
}


#[test]
fn test_is_nice() {
	assert_eq!(is_nice("qjhvhtzxzqqjkmpb"), true);
	assert_eq!(is_nice("xxyxx"), true);
	assert_eq!(is_nice("aaa"), false);
	assert_eq!(is_nice("uurcxstgmygtbstg"), false);
	assert_eq!(is_nice("ieodomkazucvgmuy"), false);
	assert_eq!(is_nice("jerbbbnxlwfvlaiw"), false);
}
