extern crate regex;
use regex::Regex;
mod input;

fn main() {
	println!("{}", num_valid(input::puzzle_input()));
	// 170
}

fn num_valid(input: &str) -> u32 {
	let mut cur_fields: Vec<String> = Vec::new();
	let mut result = 0;
	let re = Regex::new(r"([a-z]{3}):[^ ]+").unwrap();

	for line in input.lines() {
		if line == "" {
			if is_valid(&cur_fields) {
				result += 1;
			}

			cur_fields.clear();
		} else {
			for cap in re.captures_iter(line) {
				cur_fields.push(cap[1].to_string());
			}
		}
	}

	if is_valid(&cur_fields) {
		result += 1;
	}

	result
}

#[test]
fn test_num_valid() {
	/*
		0: valid
		1: invalid (missing hgt)
		2: valid (only missing cid, a special case)
		3: invalid (missing cid and byr)
	*/
	let input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in";
	assert_eq!(num_valid(input), 2);
}

fn is_valid(fields: &Vec<String>) -> bool {
	EXPECTED_FIELDS.iter().all(|f| fields.contains(&f.to_string()))
}

static EXPECTED_FIELDS: &[&str; 7] = &[
	"byr",
	"iyr",
	"eyr",
	"hgt",
	"hcl",
	"ecl",
	"pid",
	// cid intentionally excluded
];
