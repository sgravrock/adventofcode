use crate::lexer::{ReverseLexer, Token};

/*
Grammar:

Expression -> Value MaybeOperation
Value -> rparen Expression lparen | const
MaybeOperation -> operator Expression | nothing

Which is of course left associative, which is the opposite of what's needed.
The Right Thing To Do would be to write a right associative grammar, which
would be left recursive, and then rework it to eliminate the left recursion.
Or I could do the cheap & dirty thing: Run the lexer backwards, reverse the
parens (did you notice that part?), and call it a day.

Cheap & dirty means not having to think about Chomsky Normal Form while on
vacation. Cheap & dirty it is.

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
	parse_expression(&mut ReverseLexer::new(input))
}

fn parse_expression(mut lexer: &mut ReverseLexer) -> Expression {
	let lhs = parse_value(&mut lexer);
	let op = parse_maybe_operation(&mut lexer);
	Expression { lhs, op }
}

fn parse_value(mut lexer: &mut ReverseLexer) -> Value {
	let t = ensure_next(&mut lexer);
	match t {
		Token::Const(n) => Value::Const(n),
		Token::Rparen => {
			let expr = parse_expression(&mut lexer);
			require(Token::Lparen, &mut lexer);
			Value::Expression(Box::new(expr))
		},
		_ => panic!("Expected constant or left paren but got {:?}", t)
	}
}

fn parse_maybe_operation(mut lexer: &mut ReverseLexer) -> Option<Operation> {
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

fn parse_operation_rhs(
	mut lexer: &mut ReverseLexer,
	operator: Operator
) -> Operation {
	Operation {
		operator,
		rhs: Box::new(parse_expression(&mut lexer))
	}
}

fn ensure_next(lexer: &mut ReverseLexer) -> Token {
	match lexer.next() {
		Some(t) => t,
		None => panic!("Unexpected end of input")
	}
}

fn require(expected: Token, mut lexer: &mut ReverseLexer) {
	let actual = ensure_next(&mut lexer);

	if actual != expected {
		panic!("Expected {:?} but got {:?}", expected, actual);
	}
}

#[test]
fn test_parse_simple() {
	let input = "2 + 3";
	let expected = Expression {
		lhs: Value::Const(3),
		op: Some(Operation {
			operator: Operator::Add,
			rhs: Box::new(Expression {
				lhs: Value::Const(2),
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
			lhs: Value::Const(5),
			op: Some(Operation {
				operator: Operator::Mul,
				rhs: Box::new(Expression {
					lhs: Value::Const(4),
					op: None
				})
			})
		})),
		op: Some(Operation {
			operator: Operator::Add,
			rhs: Box::new(Expression {
				lhs: Value::Expression(Box::new(Expression {
					lhs: Value::Expression(Box::new(Expression {
						lhs: Value::Const(4),
						op: Some(Operation {
							operator: Operator::Add,
							rhs: Box::new(Expression {
								lhs: Value::Const(3),
								op: None
							})
						})
					})),
					op: Some(Operation {
						operator: Operator::Mul,
						rhs: Box::new(Expression {
							lhs: Value::Const(2),
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


#[test]
fn test_parse_associates_correctly() {
	let input = "2 * 3 + 4";
	// Parses as (2 * 3) + 4
	let expected = Expression {
		lhs: Value::Const(4),
		op: Some(Operation {
			operator: Operator::Add,
			rhs: Box::new(Expression {
				lhs: Value::Const(3),
				op: Some(Operation {
					operator: Operator::Mul,
					rhs: Box::new(Expression {
						lhs: Value::Const(2),
						op: None
					})
				})
			})
		})
	};
	assert_eq!(parse(input), expected);
}
