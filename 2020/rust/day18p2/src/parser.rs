use crate::lexer::{Lexer, Token};

/*
Grammar:
E -> E*T | T
T -> T+F | F
F -> (E) | const

Eliminating left recursion:
E -> TE'
E' -> *TE' | nothing
T -> FT'
T' -> +FT | nothing
F -> (E) | const

This is just the standard arithmetic grammar but with the + and *
symbols swapped.

Then rename E' to Mul and T' to Add, for clarity
*/

#[derive(PartialEq, Eq, Debug)]
pub struct Expr {
	pub term: Term,
	pub mul: Option<Mul>
}

#[derive(PartialEq, Eq, Debug)]
pub struct Mul {
	pub term: Term,
	pub mul: Option<Box<Mul>>
}

#[derive(PartialEq, Eq, Debug)]
pub struct Term {
	pub factor: Factor,
	pub add: Option<Add>
}

#[derive(PartialEq, Eq, Debug)]
pub struct Add {
	pub factor: Factor,
	pub add: Option<Box<Add>>
}

#[derive(PartialEq, Eq, Debug)]
pub enum Factor {
	Expr(Box<Expr>), // parenthesized
	Const(i64)
}


pub fn parse(input: &str) -> Expr {
	let mut lexer = Lexer::new(input);
	let result = parse_expr(&mut lexer);
	assert!(lexer.next().is_none());
	result
}

fn parse_expr(mut lexer: &mut Lexer) -> Expr {
	let term = parse_term(&mut lexer);
	let mul = parse_mul(&mut lexer);
	Expr { term, mul }
}

fn parse_mul(mut lexer: &mut Lexer) -> Option<Mul> {
	let t = lexer.next();
	match t {
		Some(Token::Mul) => {
			let term = parse_term(&mut lexer);
			let mul = parse_mul(&mut lexer).map(|m| Box::new(m));
			Some(Mul { term, mul })
		},
		Some(t) => {
			lexer.put_back(t);
			None
		},
		None => None
	}
}

fn parse_term(mut lexer: &mut Lexer) -> Term {
	let factor = parse_factor(&mut lexer);
	let add = parse_add(&mut lexer);
	Term { factor, add }
}

fn parse_add(mut lexer: &mut Lexer) -> Option<Add> {
	match lexer.next() {
		Some(Token::Add) => {
			let factor = parse_factor(&mut lexer);
			let add = parse_add(&mut lexer).map(|a| Box::new(a));
			Some(Add { factor, add })
		},
		Some(t) => {
			lexer.put_back(t);
			None
		},
		None => None
	}
}

fn parse_factor(mut lexer: &mut Lexer) -> Factor {
	let t = ensure_next(&mut lexer);
	match t {
		Token::Lparen => {
			let expr = parse_expr(&mut lexer);
			require(Token::Rparen, &mut lexer);
			Factor::Expr(Box::new(expr))
		},
		Token::Const(c) => Factor::Const(c),
		_ => panic!("Expected lparen or const but got {:?}", t)
	}
}

fn ensure_next(lexer: &mut Lexer) -> Token {
	match lexer.next() {
		Some(t) => t,
		None => panic!("Unexpected end of input")
	}
}

fn require(expected: Token, mut lexer: &mut Lexer) {
	let actual = ensure_next(&mut lexer);

	if actual != expected {
		panic!("Expected {:?} but got {:?}", expected, actual);
	}
}

#[test]
fn test_parse_simple() {
	let input = "2 + 3";
	let expected = Expr {
		term: Term {
			factor: Factor::Const(2),
			add: Some(Add {
				factor: Factor::Const(3),
				add: None
			})
		},
		mul: None
	};
	assert_eq!(parse(input), expected);
}

#[test]
fn test_parse_precedence() {
	let input = "1 + 2 * 3";
	let expected = Expr {
		term: Term {
			factor: Factor::Const(1),
			add: Some(Add {
				factor: Factor::Const(2),
				add: None
			})
		},
		mul: Some(Mul {
			term: Term {
				factor: Factor::Const(3),
				add: None
			},
			mul: None
		})
	};
	assert_eq!(parse(input), expected);
}
