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
	has_vowels(s) && has_doubles(s) && !has_bad_strings(s)
}

fn has_vowels(s: &str) -> bool {
	s.chars().filter(|&c| { "aeiou".contains(c) }).count() >= 3
}

fn has_doubles(s: &str) -> bool {
	let chars: Vec<_> = s.chars().collect();

	for i in 1..chars.len() {
		if chars[i] == chars[i - 1] {
			return true;
		}
	}

	false
}

fn has_bad_strings(s: &str) -> bool {
	["xy", "ab", "cd", "pq"].iter().any(|bad| { s.contains(bad) })
}


#[test]
fn test_is_nice() {
	assert_eq!(is_nice("ugknbfddgicrmopn"), true);
	assert_eq!(is_nice("aaa"), true);
	assert_eq!(is_nice("aaz"), false);
	assert_eq!(is_nice("bbb"), false);
	assert_eq!(is_nice("jchzalrnumimnmhp"), false);
	assert_eq!(is_nice("haegwjzuvuyypxyu"), false);
	assert_eq!(is_nice("haegwjzuvuyypabu"), false);
	assert_eq!(is_nice("haegwjzuvuyypcdu"), false);
	assert_eq!(is_nice("haegwjzuvuyyppqu"), false);
	assert_eq!(is_nice("kujsaiqojopvrygg"), true);
}
