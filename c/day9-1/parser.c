#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/*
document: group TT_END
group: TT_OPEN_BRACE group-body TT_CLOSE_BRACE
group-body: list-head list-tail | nothing
list-head: group | garbage
list-tail: TT_COMMA list-head | nothing
garbage: TT_OPEN_ANGLE garbage-contents TT_CLOSE_ANGLE
garbage-contents: garbage-atom garbage-contents | nothing
garbage-atom: (any token except TT_CLOSE_ANGLE)
*/


static bool maybe_group(Parser parser, int group_depth);
static void group_body(Parser parser, int group_depth);
static bool maybe_list_head(Parser parser, int group_depth);
static void list_tail(Parser parser, int group_depth);

void parse(Parser parser) {
	if (!maybe_group(parser, 0)) {
		fprintf(stderr, "Parse error: expected input to start with {\n");
		parser.fail();
		return;
	}

	Token t = lexer_next(parser.lexer);

	if (t.type != TT_END) {
		fprintf(stderr, "Parse error: expected end of input but got %c\n", t.c);
		parser.fail();
	}
}

static bool maybe_group(Parser parser, int group_depth) {
	Token t = lexer_peek(parser.lexer);

	if (t.type != TT_OPEN_BRACE) {
		return false;
	}

	lexer_next(parser.lexer);
	parser.emit_group(group_depth + 1);
	group_body(parser, group_depth + 1);

	Token t2 = lexer_next(parser.lexer);
	if (t2.type != TT_CLOSE_BRACE) {
		fprintf(stderr, "Parse error: Expected } but got %c\n", t2.c);
		parser.fail();
	}

	return true;
}

static void group_body(Parser parser, int group_depth) {
	if (maybe_list_head(parser, group_depth)) {
		list_tail(parser, group_depth);
	}
}

static bool maybe_list_head(Parser parser, int group_depth) {
	// TODO: could also be garbage
	if (!maybe_group(parser, group_depth)) {
		return false;
	}

	/*
	Token t = lexer_peek(parser.lexer);

	if (t.type != 
	*/
	return false;
}

static void list_tail(Parser parser, int group_depth) {
	if (lexer_peek(parser.lexer).type != TT_COMMA) {
		return;
	}

	lexer_next(parser.lexer);

	if (!maybe_list_head(parser, group_depth)) {
		fprintf(stderr, "Parse error: expected a list item but got %c\n",
			lexer_peek(parser.lexer).c);
	}
}
