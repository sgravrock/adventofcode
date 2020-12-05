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
	VALIDATORS.iter().all(|f| f(&fields))
}

fn byr_valid(fields: &Vec<Field>) -> bool {
	is_num_in_range(&fields, "byr", 1920..=2002)
}

#[test]
fn test_byr_valid() {
	assert_eq!(byr_valid(&vec![]), false);
	assert_eq!(byr_valid(&vec![Field::new("byr", "not a number")]), false);
	assert_eq!(byr_valid(&vec![Field::new("byr", "1920")]), true);
	assert_eq!(byr_valid(&vec![Field::new("byr", "2002")]), true);
	assert_eq!(byr_valid(&vec![Field::new("byr", "2003")]), false);
	assert_eq!(byr_valid(&vec![Field::new("byr", "1919")]), false);
}

fn iyr_valid(fields: &Vec<Field>) -> bool {
	is_num_in_range(&fields, "iyr", 2010..=2020)
}

fn eyr_valid(fields: &Vec<Field>) -> bool {
	is_num_in_range(&fields, "eyr", 2010..=2030)
}

fn hgt_valid(fields: &Vec<Field>) -> bool {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^([\d]+)(in|cm)$").unwrap();
	}

	fields.iter().find(|f| f.name == "hgt")
		.and_then(|f| RE.captures(&f.value))
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
	assert_eq!(hgt_valid(&vec![]), false);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "tall")]), false);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "60furlongs")]), false);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "59in")]), true);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "76in")]), true);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "58in")]), false);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "77in")]), false);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "150cm")]), true);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "193cm")]), true);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "149cm")]), false);
	assert_eq!(hgt_valid(&vec![Field::new("hgt", "194cm")]), false);
}

fn hcl_valid(fields: &Vec<Field>) -> bool {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
	}

	fields.iter().find(|f| f.name == "hcl")
		.and_then(|f| Some(RE.is_match(&f.value)))
		.unwrap_or(false)
}

#[test]
fn test_hcl_valid() {
	assert_eq!(hcl_valid(&vec![]), false);
	assert_eq!(hcl_valid(&vec![Field::new("hcl", "aaaaaa")]), false);
	assert_eq!(hcl_valid(&vec![Field::new("hcl", "#abcdef")]), true);
	assert_eq!(hcl_valid(&vec![Field::new("hcl", "#012345")]), true);
	assert_eq!(hcl_valid(&vec![Field::new("hcl", "#6789ab")]), true);
	assert_eq!(hcl_valid(&vec![Field::new("hcl", "#abcdeff")]), false);
	assert_eq!(hcl_valid(&vec![Field::new("hcl", "#abcde")]), false);
	assert_eq!(hcl_valid(&vec![Field::new("hcl", "#ghijkl")]), false);
}

fn ecl_valid(fields: &Vec<Field>) -> bool {
	fields.iter().find(|f| f.name == "ecl")
		.and_then(|f| {
			Some(f.value == "amb" || f.value == "blu" || f.value == "brn" ||
				f.value == "gry" || f.value == "grn" || f.value == "hzl" ||
				f.value == "oth")
		})
		.unwrap_or(false)
}

#[test]
fn test_ecl_valid() {
	assert_eq!(ecl_valid(&vec![]), false);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "amb")]), true);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "blu")]), true);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "brn")]), true);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "gry")]), true);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "grn")]), true);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "hzl")]), true);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "oth")]), true);
	assert_eq!(ecl_valid(&vec![Field::new("ecl", "anything else")]), false);
}

fn pid_valid(fields: &Vec<Field>) -> bool {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^[\d]{9}$").unwrap();
	}

	fields.iter().find(|f| f.name == "pid")
		.and_then(|f| Some(RE.is_match(&f.value)))
		.unwrap_or(false)
}

#[test]
fn test_pid_valid() {
	assert_eq!(pid_valid(&vec![]), false);
	assert_eq!(pid_valid(&vec![Field::new("pid", "012345678")]), true);
	assert_eq!(pid_valid(&vec![Field::new("pid", "123456789")]), true);
	assert_eq!(pid_valid(&vec![Field::new("pid", "12345678")]), false);
	assert_eq!(pid_valid(&vec![Field::new("pid", "0123456789")]), false);
	assert_eq!(pid_valid(&vec![Field::new("pid", "a12345678")]), false);
}


fn is_num_in_range(
	fields: &Vec<Field>,
	field_name: &str,
	range: RangeInclusive<u32>
) -> bool {
	fields.iter().find(|f| f.name == field_name)
		.and_then(|target_field| target_field.value.parse::<u32>().ok())
		.and_then(|val| Some(range.contains(&val)))
		.unwrap_or(false)
}

// TODO: can this type be simplfiied?
static VALIDATORS: &[for<'r> fn(&'r Vec<Field>) -> bool; 7] = &[
	byr_valid,
	iyr_valid,
	eyr_valid,
	hgt_valid,
	hcl_valid,
	ecl_valid,
	pid_valid,
];
