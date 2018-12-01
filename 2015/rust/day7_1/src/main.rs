#[derive(PartialEq, Eq, Debug)]
enum Atom<'a> {
	Value(i32),
	Ref(&'a str),
}

#[derive(PartialEq, Eq, Debug)]
enum Expr<'a> {
	Atom(Atom<'a>),
	And(Atom<'a>, Atom<'a>),
	Or(Atom<'a>, Atom<'a>),
	Lshift(Atom<'a>, Atom<'a>),
	Rshift(Atom<'a>, Atom<'a>),
}

#[derive(PartialEq, Eq, Debug)]
struct Line<'a> {
	name: &'a str,
	expr: Expr<'a>,
}

fn main() {
    println!("Hello, world!");
}

fn parse_line(s: &str) -> Line {
	let tokens: Vec<&str> = s.split_whitespace().collect();

	if tokens[1] == "->" {
		Line { name: tokens[2], expr: Expr::Atom(parse_atom(tokens[0])) }
	} else if tokens[1] == "AND" {
		Line {
			name: tokens[4],
			expr: Expr::And(parse_atom(tokens[0]), parse_atom(tokens[2]))
		}
	} else if tokens[1] == "OR" {
		Line {
			name: tokens[4],
			expr: Expr::Or(parse_atom(tokens[0]), parse_atom(tokens[2]))
		}
	} else if tokens[1] == "LSHIFT" {
		Line {
			name: tokens[4],
			expr: Expr::Lshift(parse_atom(tokens[0]), parse_atom(tokens[2]))
		}
	} else if tokens[1] == "RSHIFT" {
		Line {
			name: tokens[4],
			expr: Expr::Rshift(parse_atom(tokens[0]), parse_atom(tokens[2]))
		}
	} else {
		Line { name: "foo", expr: Expr::Atom(parse_atom("12")) }
	}
}

fn parse_atom(token: &str) -> Atom {
	match token.parse::<i32>() {
		Ok(n) => Atom::Value(n),
		Err(_) => Atom::Ref(token)
	}
}

#[test]
fn test_parse_line() {
	assert_eq!(parse_line("123 -> x"),
		Line { name: "x", expr: Expr::Atom(Atom::Value(123)) });
	assert_eq!(parse_line("a -> x"),
		Line { name: "x", expr: Expr::Atom(Atom::Ref("a")) });
	assert_eq!(parse_line("x AND 5 -> d"),
		Line { name: "d", expr: Expr::And(Atom::Ref("x"), Atom::Value(5)) });
	assert_eq!(parse_line("x OR 5 -> d"),
		Line { name: "d", expr: Expr::Or(Atom::Ref("x"), Atom::Value(5)) });
	assert_eq!(parse_line("x LSHIFT 5 -> d"),
		Line { name: "d", expr: Expr::Lshift(Atom::Ref("x"), Atom::Value(5)) });
	assert_eq!(parse_line("x RSHIFT 5 -> d"),
		Line { name: "d", expr: Expr::Rshift(Atom::Ref("x"), Atom::Value(5)) });
}

/*
fn evaluate(e: &Expr) -> Option<i32> {
	match *e {
		Expr::Value(n) => Some(n),
		_ => None
	}
}

#[test]
fn test_evaluate() {
	let v = Expr::Value(42);
	assert_eq!(evaluate(&v), Some(42));
	assert_eq!(evaluate(&Expr::Ref("foo")), None);
	assert_eq!(evaluate(&Expr::Ref("foo")), None);
	//assert_eq!(evaluate(&Expr::And(&v, &v)), None);
	//assert_eq!(evaluate(&Expr::Lshift(&v, &v)), None);
}
*/
