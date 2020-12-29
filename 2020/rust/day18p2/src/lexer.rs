use std::str::Chars;
use std::iter::Rev;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Token { Const(i32), Lparen, Rparen, Add, Mul }

pub struct ReverseLexer<'a> {
	chars: Rev<Chars<'a>>,
	putback: Option<Token>,
}

impl <'a> ReverseLexer<'a> {
	pub fn new(input: &'a str) -> ReverseLexer<'a> {
		ReverseLexer {
			chars: input.chars().rev(),
			putback: None
		}
	}

	pub fn put_back(&mut self, tok: Token) {
		if self.putback.is_some() {
			panic!("Can't put_back twice without consuming in between");
		}

		self.putback = Some(tok);
	}

	fn next_from_putback(&mut self) -> Option<Token> {
		match self.putback {
			Some(t) => {
				self.putback = None;
				Some(t)
			},
			None => None
		}
	}

	fn next_from_chars(&mut self) -> Option<Token> {
		match self.chars.next() {
			Some('(') => Some(Token::Lparen),
			Some(')') => Some(Token::Rparen),
			Some('+') => Some(Token::Add),
			Some('*') => Some(Token::Mul),
			Some('0') => Some(Token::Const(0)),
			Some('1') => Some(Token::Const(1)),
			Some('2') => Some(Token::Const(2)),
			Some('3') => Some(Token::Const(3)),
			Some('4') => Some(Token::Const(4)),
			Some('5') => Some(Token::Const(5)),
			Some('6') => Some(Token::Const(6)),
			Some('7') => Some(Token::Const(7)),
			Some('8') => Some(Token::Const(8)),
			Some('9') => Some(Token::Const(9)),
			Some(' ') => self.next(),
			Some(c) => panic!("Unrecognized input char '{}'", c),
			None => None,
		}
	}
}

impl Iterator for ReverseLexer<'_> {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		let r = self.next_from_putback().or_else(|| self.next_from_chars());
		println!("lexer emitting {:?}", r);
		r
	}
}

#[test]
fn test_lex() {
	let lexer = ReverseLexer::new("2 * (3 + 4)");
	let tokens: Vec<Token> = lexer.collect();
	assert_eq!(tokens, vec![Token::Rparen, Token::Const(4), Token::Add,
		Token::Const(3), Token::Lparen, Token::Mul, Token::Const(2)]);
}

#[test]
fn test_put_back() {
	let mut lexer = ReverseLexer::new("* 2");
	let t = lexer.next().unwrap();
	assert_eq!(t, Token::Const(2));
	lexer.put_back(t);
	assert_eq!(lexer.next(), Some(Token::Const(2)));
	assert_eq!(lexer.next(), Some(Token::Mul));
}
