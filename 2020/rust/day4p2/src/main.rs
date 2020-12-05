#[macro_use] extern crate lazy_static;
extern crate regex;
use regex::Regex;
use std::ops::RangeInclusive;
mod input;

fn main() {
	println!("{}", num_valid(input::puzzle_input()));
	// 103
}

#[derive(Debug)]
struct Field {
	name: String,
	value: String,
}

impl Field {
	fn new(name: &str, value: &str) -> Field {
		Field {
			name: name.to_string(),
			value: value.to_string()
		}
	}
}

struct Validator {
	field_name: &'static str,
	predicate: fn(&str) -> bool,
}

impl Validator {
	fn is_valid(&self, fields: &Vec<Field>) -> bool {
		fields.iter().find(|f| f.name == self.field_name)
			.and_then(|f| Some((self.predicate)(&f.value)))
			.unwrap_or(false)
	}
}

fn num_valid(input: &str) -> u32 {
	let mut cur_fields: Vec<Field> = Vec::new();
	let mut result = 0;
	let re = Regex::new(r"([a-z]{3}):([^ ]+)").unwrap();

	for line in input.lines() {
		if line == "" {
			if is_valid(&cur_fields) {
				result += 1;
			}

			cur_fields.clear();
		} else {
			for cap in re.captures_iter(line) {
				cur_fields.push(Field::new(
					&cap[1].to_string(),
					&cap[2].to_string()
				));
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
	let invalids = "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007";
	let valids = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719";

	assert_eq!(num_valid(invalids), 0);
	assert_eq!(num_valid(valids), 4);
}

fn is_valid(fields: &Vec<Field>) -> bool {
	VALIDATORS.iter().all(|v| v.is_valid(&fields))
}

fn byr_valid(value: &str) -> bool {
	is_num_in_range(value, 1920..=2002)
}

#[test]
fn test_byr_valid() {
	assert_eq!(byr_valid("not a number"), false);
	assert_eq!(byr_valid("1920"), true);
	assert_eq!(byr_valid("2002"), true);
	assert_eq!(byr_valid("2003"), false);
	assert_eq!(byr_valid("1919"), false);
}

fn iyr_valid(value: &str) -> bool {
	is_num_in_range(value, 2010..=2020)
}

fn eyr_valid(value: &str) -> bool {
	is_num_in_range(value, 2010..=2030)
}

fn hgt_valid(value: &str) -> bool {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^([\d]+)(in|cm)$").unwrap();
	}

	RE.captures(&value)
		.and_then(|caps| {
			caps[1].parse::<u32>().ok().and_then(|v| {
				Some((v, caps[2].to_string()))
			})
		})
		.and_then(|(val, units)| {
			let range = if units == "in" {
				59..=76
			} else {
				150..=193
			};
			Some(range.contains(&val))
		})
		.unwrap_or(false)
}

#[test]
fn test_hgt_valid() {
	assert_eq!(hgt_valid("tall"), false);
	assert_eq!(hgt_valid("60furlongs"), false);
	assert_eq!(hgt_valid("59in"), true);
	assert_eq!(hgt_valid("76in"), true);
	assert_eq!(hgt_valid("58in"), false);
	assert_eq!(hgt_valid("77in"), false);
	assert_eq!(hgt_valid("150cm"), true);
	assert_eq!(hgt_valid("193cm"), true);
	assert_eq!(hgt_valid("149cm"), false);
	assert_eq!(hgt_valid("194cm"), false);
}

fn hcl_valid(value: &str) -> bool {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
	}

	RE.is_match(&value)
}

#[test]
fn test_hcl_valid() {
	assert_eq!(hcl_valid("aaaaaa"), false);
	assert_eq!(hcl_valid("#abcdef"), true);
	assert_eq!(hcl_valid("#012345"), true);
	assert_eq!(hcl_valid("#6789ab"), true);
	assert_eq!(hcl_valid("#abcdeff"), false);
	assert_eq!(hcl_valid("#abcde"), false);
	assert_eq!(hcl_valid("#ghijkl"), false);
}

fn ecl_valid(value: &str) -> bool {
	value == "amb" || value == "blu" || value == "brn" ||
		value == "gry" || value == "grn" || value == "hzl" ||
		value == "oth"
}

#[test]
fn test_ecl_valid() {
	assert_eq!(ecl_valid("amb"), true);
	assert_eq!(ecl_valid("blu"), true);
	assert_eq!(ecl_valid("brn"), true);
	assert_eq!(ecl_valid("gry"), true);
	assert_eq!(ecl_valid("grn"), true);
	assert_eq!(ecl_valid("hzl"), true);
	assert_eq!(ecl_valid("oth"), true);
	assert_eq!(ecl_valid("anything else"), false);
}

fn pid_valid(value: &str) -> bool {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^[\d]{9}$").unwrap();
	}

	RE.is_match(&value)
}

#[test]
fn test_pid_valid() {
	assert_eq!(pid_valid("012345678"), true);
	assert_eq!(pid_valid("123456789"), true);
	assert_eq!(pid_valid("12345678"), false);
	assert_eq!(pid_valid("0123456789"), false);
	assert_eq!(pid_valid("a12345678"), false);
}


fn is_num_in_range(
	value: &str,
	range: RangeInclusive<u32>
) -> bool {
	match value.parse::<u32>() {
		Ok(n) => range.contains(&n),
		Err(_) => false
	}
}

static VALIDATORS: &[Validator; 7] = &[
	Validator { field_name: "byr", predicate: byr_valid },
	Validator { field_name: "iyr", predicate: iyr_valid },
	Validator { field_name: "eyr", predicate: eyr_valid },
	Validator { field_name: "hgt", predicate: hgt_valid },
	Validator { field_name: "hcl", predicate: hcl_valid },
	Validator { field_name: "ecl", predicate: ecl_valid },
	Validator { field_name: "pid", predicate: pid_valid },
];
