mod input;
use std::collections::HashMap;

fn main() {
	let (grammar, messages) = parse_input(input::puzzle_input());
	let n_matches = messages.iter()
		.filter(|m| grammar.matches(m, 0))
		.count();
	println!("{}", n_matches);
	// 263
}

fn parse_input<'a>(input: &'a str) -> (Grammar, Vec<&'a str>) {
	let mut parts = input.split("\n\n");
	let grammar = Grammar::parse(parts.next().unwrap());
	let messages = parts.next().unwrap().lines().collect();
	(grammar, messages)
}

#[derive(PartialEq, Eq, Debug)]
struct Grammar {
	rules: HashMap<usize, Vec<Production>>
}

impl Grammar {
	fn parse(input: &str) -> Grammar {
		let rules = input.lines()
			.map(|line| {
				let mut tokens = line.split(": ");
				let i: usize = tokens.next().unwrap().parse().unwrap();
				let rest = tokens.next().unwrap();
				(i, rest)
			})
			.map(|(i, s)| {
				let rule = s.split(" | ")
					.map(|p| {
						if p.contains("\"") {
							let c = p.replace("\"", "").chars().next().unwrap();
							Production::Terminal(c)
						} else {
							let is = p.split(" ")
								.map(|ns| ns.parse::<usize>().unwrap())
								.collect();
							Production::Nonterminal(is)
						}
					})
					.collect();
				(i, rule)
			})
			.collect();
		Grammar { rules }
	}

	fn matches(&self, msg: &str, rule_ix: usize) -> bool {
		let chars = msg.chars().collect();
		self.matches2(rule_ix, &chars, 0)
			.iter()
			.any(|next_char_ix| *next_char_ix == msg.len())
	}

	fn matches2(
		&self, rule_ix: usize,
		chars: &Vec<char>,
		char_ix: usize
) -> Vec<usize> {
		self.rules[&rule_ix].iter()
			.flat_map(|production| {
				match production {
					Production::Terminal(c) => {
						if char_ix < chars.len() && chars[char_ix] == *c {
							vec![char_ix + 1]
						} else {
							vec![]
						}
					},
					Production::Nonterminal(rule_ixs) => {
						rule_ixs.iter()
							.fold(vec![char_ix], |char_ixs, ri| {
								char_ixs.iter()
									.flat_map(|ci| self.matches2(*ri, &chars, *ci))
									.collect()
								})
					}
				}
			})
			.collect()
	}
}

#[derive(PartialEq, Eq, Debug)]
enum Production {
	Nonterminal(Vec<usize>),
	Terminal(char)
}

#[test]
fn test_grammar_parse() {
	let mut expected_rules: HashMap<usize, Vec<Production>> = HashMap::new();
	expected_rules.insert(0, vec![
		Production::Nonterminal(vec![2, 1, 3]),
	]);
	expected_rules.insert(1, vec![
		Production::Nonterminal(vec![2, 3]),
		Production::Nonterminal(vec![3, 2]),
	]);
	expected_rules.insert(2, vec![
		Production::Terminal('a'),
	]);
	expected_rules.insert(3, vec![
		Production::Terminal('b'),
	]);
	let actual = Grammar::parse("0: 2 1 3
1: 2 3 | 3 2
2: \"a\"
3: \"b\"");
	assert_eq!(actual, Grammar { rules: expected_rules });
}

#[test]
fn test_grammar_matches() {
	let grammar = Grammar::parse("0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"");
	assert_eq!(grammar.matches("aaaabb", 0), true);
	assert_eq!(grammar.matches("aaabab", 0), true);
	assert_eq!(grammar.matches("abbabb", 0), true);
	assert_eq!(grammar.matches("abbbab", 0), true);
	assert_eq!(grammar.matches("aabaab", 0), true);
	assert_eq!(grammar.matches("aabbbb", 0), true);
	assert_eq!(grammar.matches("abaaab", 0), true);
	assert_eq!(grammar.matches("ababbb", 0), true);
	assert_eq!(grammar.matches("bbabbb", 0), false);
	assert_eq!(grammar.matches("ababbbx", 0), false);
}

#[test]
fn test_allows_non_consecutive_rule_ixs() {
	let (grammar, messages) = parse_input("42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31 | 42 11 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42 | 42 8
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba");
	let expected_matches = vec![
		"bbabbbbaabaabba",
		"babbbbaabbbbbabbbbbbaabaaabaaa",
		"aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
		"bbbbbbbaaaabbbbaaabbabaaa",
		"bbbababbbbaaaaaaaabbababaaababaabab",
		"ababaaaaaabaaab",
		"ababaaaaabbbaba",
		"baabbaaaabbaaaababbaababb",
		"abbbbabbbbaaaababbbbbbaaaababb",
		"aaaaabbaabaaaaababaa",
		"aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
		"aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
	];
	let actual_matches: Vec<&str> = messages.iter()
		.filter(|m| grammar.matches(m, 0))
		.map(|m| *m)
		.collect();
	assert_eq!(actual_matches, expected_matches);

}
