mod lexer;
mod parser;
mod input;
use crate::parser::{parse, Expr, Term, Factor, Mul, Add};

fn main() {
	println!("{}", solve(input::puzzle_input()));
	// 321176691637769
}

fn solve(input: &str) -> i64 {
	input.lines()
		.map(|line| eval_expr(parse(line)))
		.sum()
}

fn eval_expr(e: Expr) -> i64 {
	println!("{:#?}", e);
	let lhs = eval_term(e.term);
	match e.mul {
		Some(mul) => eval_mul(lhs, mul),
		None => lhs
	}
}

fn eval_term(t: Term) -> i64 {
	let lhs = eval_factor(t.factor);
	match t.add {
		Some(add) => eval_add(lhs, add),
		None => lhs
	}
}

fn eval_factor(f: Factor) -> i64 {
	match f {
		Factor::Expr(e) => eval_expr(*e),
		Factor::Const(c) => c
	}
}

fn eval_mul(lhs: i64, mul: Mul) -> i64 {
	println!("eval_mul({}, {:?})", lhs, mul);
	let x = lhs * eval_term(mul.term);
	match mul.mul {
		Some(m) => eval_mul(x, *m),
		None => x
	}
}

fn eval_add(lhs: i64, add: Add) -> i64 {
	let x = lhs + eval_factor(add.factor);
	match add.add {
		Some(m) => eval_add(x, *m),
		None => x
	}
}

#[test]
fn test_eval_expr() {
	assert_eq!(eval_expr(parse("1 + 2 * 3")), 9);
	assert_eq!(eval_expr(parse("1 + 2 * 3 + 4 * 5 + 6")), 231);
	assert_eq!(eval_expr(parse("1 + (2 * 3) + (4 * (5 + 6))")), 51);
	assert_eq!(eval_expr(parse("5 + (8 * 3 + 9 + 3 * 4 * 3)")), 1445);
	assert_eq!(
		eval_expr(parse("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")),
		669060
	);
	assert_eq!(
		eval_expr(parse("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")),
		23340
	);
}
