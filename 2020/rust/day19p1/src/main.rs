mod input;
use itertools::Itertools;

fn main() {
	let (grammar, messages) = parse_input(input::puzzle_input());
	let n_matches = messages.iter()
		.filter(|m| grammar.matches(m, 0))
		.count();
	println!("{}", n_matches);
	// 147
}

fn parse_input<'a>(input: &'a str) -> (Grammar, Vec<&'a str>) {
	let mut parts = input.split("\n\n");
	let grammar = Grammar::parse(parts.next().unwrap());
	let messages = parts.next().unwrap().lines().collect();
	(grammar, messages)
}

#[derive(PartialEq, Eq, Debug)]
struct Grammar {
	rules: Vec<Vec<Production>>
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
			.sorted_by_key(|(i, _)| i.clone())
			.map(|(_, s)| {
				s.split(" | ")
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
					.collect()
			})
			.collect();
		Grammar { rules }
	}

	fn matches(&self, msg: &str, rule_ix: usize) -> bool {
		let chars = msg.chars().collect();
		match self.matches2(rule_ix, &chars, 0) {
			None => false,
			Some(n) => n == msg.len()
		}
	}

	fn matches2(
		&self, rule_ix: usize,
		chars: &Vec<char>,
		char_ix: usize
) -> Option<usize> {
		self.rules[rule_ix].iter()
			.filter_map(|production| {
				match production {
					Production::Terminal(c) => {
						if chars[char_ix] == *c {
							Some(char_ix + 1)
						} else {
							None
						}
					},
					Production::Nonterminal(rule_ixs) => {
						rule_ixs.iter()
							.fold(Some(char_ix), |maybe_ci, ri| {
								match maybe_ci {
									Some(ci) => self.matches2(*ri, &chars, ci),
									None => None
								}
							})
					}
				}
			})
			.take(1)
			.next()
	}
}

#[derive(PartialEq, Eq, Debug)]
enum Production {
	Nonterminal(Vec<usize>),
	Terminal(char)
}

#[test]
fn test_grammar_parse() {
	let expected = Grammar {
		rules: vec![
			vec![ // 0
				Production::Nonterminal(vec![2, 1, 3]),
			],
			vec![ // 1
				Production::Nonterminal(vec![2, 3]),
				Production::Nonterminal(vec![3, 2]),
			],
			vec![ // 2
				Production::Terminal('a'),
			],
			vec![ // 3
				Production::Terminal('b'),
			],
		]
	};
	let actual = Grammar::parse("0: 2 1 3
1: 2 3 | 3 2
2: \"a\"
3: \"b\"");
	assert_eq!(actual, expected);
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
