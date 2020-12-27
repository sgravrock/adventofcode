mod lexer;
mod parser;
mod input;
use crate::parser::{parse, Expression, Value, Operation, Operator};

fn main() {
	println!("{}", solve(input::puzzle_input()));
	//50891122863118 is too high
}

fn solve(input: &str) -> i64 {
	input.lines()
		.map(|line| evaluate_expr(parse(line)))
		.sum()
}

fn evaluate_expr(expr: Expression) -> i64 {
	let lhsval = match expr.lhs {
		Value::Expression(e) => evaluate_expr(*e),
		Value::Const(n) => n as i64
	};
	match expr.op {
		Some(op) => evaluate_op(lhsval, op),
		None => lhsval
	}
}

fn evaluate_op(lhsval: i64, op: Operation) -> i64 {
	let rhsval = evaluate_expr(*op.rhs);
	match op.operator {
		Operator::Add => lhsval + rhsval,
		Operator::Mul => lhsval * rhsval,
	}
}


#[test]
fn test_evaluate_expr() {
	assert_eq!(evaluate_expr(parse("2 * 3 + (4 * 5)")), 26);
	assert_eq!(evaluate_expr(parse("5 + (8 * 3 + 9 + 3 * 4 * 3)")), 437);
	assert_eq!(evaluate_expr(parse("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")), 12240);
	assert_eq!(evaluate_expr(parse("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")), 13632);
}
