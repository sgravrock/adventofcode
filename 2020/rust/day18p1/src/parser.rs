use crate::lexer::{Lexer, Token};

/*
Grammar:

Expression -> Value MaybeOperation
Value -> lparen Expression rparen | const
MaybeOperation -> operator Expression | nothing
*/

#[derive(PartialEq, Eq, Debug)]
pub enum Operator { Add, Mul }

#[derive(PartialEq, Eq, Debug)]
pub struct Expression {
	pub lhs: Value,
	pub op: Option<Operation>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Value {
	Expression(Box<Expression>),
	Const(i32),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Operation {
	pub operator: Operator,
	pub rhs: Box<Expression>,
}

pub fn parse(input: &str) -> Expression {
	parse_expression(&mut Lexer::new(input))
}

fn parse_expression(mut lexer: &mut Lexer) -> Expression {
	let lhs = parse_value(&mut lexer);
	let op = parse_maybe_operation(&mut lexer);
	Expression { lhs, op }
}

fn parse_value(mut lexer: &mut Lexer) -> Value {
	let t = ensure_next(&mut lexer);
	match t {
		Token::Const(n) => Value::Const(n),
		Token::Lparen => {
			let expr = parse_expression(&mut lexer);
			require(Token::Rparen, &mut lexer);
			Value::Expression(Box::new(expr))
		},
		_ => panic!("Expected constant or left paren but got {:?}", t)
	}
}

fn parse_maybe_operation(mut lexer: &mut Lexer) -> Option<Operation> {
	match lexer.next() {
		None => None,
		Some(t) => match t {
			Token::Add => Some(parse_operation_rhs(&mut lexer, Operator::Add)),
			Token::Mul => Some(parse_operation_rhs(&mut lexer, Operator::Mul)),
			_ => {
				lexer.put_back(t);
				None
			}
		}
	}
}

fn parse_operation_rhs(mut lexer: &mut Lexer, operator: Operator) -> Operation {
	Operation {
		operator,
		rhs: Box::new(parse_expression(&mut lexer))
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
	let expected = Expression {
		lhs: Value::Const(2),
		op: Some(Operation {
			operator: Operator::Add,
			rhs: Box::new(Expression {
				lhs: Value::Const(3),
				op: None
			})
		})
	};
	assert_eq!(parse(input), expected);
}

#[test]
fn test_parse() {
	let input = "(2 * (3 + 4)) + (4 * 5)";
	let expected = Expression {
		lhs: Value::Expression(Box::new(Expression {
			lhs: Value::Const(2),
			op: Some(Operation {
				operator: Operator::Mul,
				rhs: Box::new(Expression {
					lhs: Value::Expression(Box::new(Expression {
						lhs: Value::Const(3),
						op: Some(Operation {
							operator: Operator::Add,
							rhs: Box::new(Expression {
								lhs: Value::Const(4),
								op: None
							})
						})
					})),
					op: None
				})
			})
		})),
		op: Some(Operation {
			operator: Operator::Add,
			rhs: Box::new(Expression {
				lhs: Value::Expression(Box::new(Expression {
					lhs: Value::Const(4),
					op: Some(Operation {
						operator: Operator::Mul,
						rhs: Box::new(Expression {
							lhs: Value::Const(5),
							op: None
						})
					})
				})),
				op: None
			})
		})
	};
	assert_eq!(parse(input), expected);
}

