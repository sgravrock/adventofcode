#include "lexer.h"
#include <stdlib.h>

struct lexer {
	const char *nextchar;
};

static char nextchar(Lexer *lexer);


Lexer *lexer_make(const char *input) {
	Lexer *result = malloc(sizeof *result);

	if (result) {
		result->nextchar = input;
	}

	return result;
}

void lexer_destroy(Lexer *lexer) {
	free(lexer);
}

Token lexer_peek(Lexer *lexer) {
	Token t;
	t.c = *(lexer->nextchar);

	switch (t.c) {
		case '{':
			t.type = TT_OPEN_BRACE;
			break;
		case '}':
			t.type = TT_CLOSE_BRACE;
			break;
		case '<':
			t.type = TT_OPEN_ANGLE;
			break;
		case '>':
			t.type = TT_CLOSE_ANGLE;
			break;
		case '!':
			t.type = TT_CANCELATION;
			t.c = lexer->nextchar[1];
			break;
		case ',':
			t.type = TT_COMMA;
			break;
		case '\0':
			t.type = TT_END;
			break;
		default:
			t.type = TT_OTHER;
	}

	return t;
}

Token lexer_next(Lexer *lexer) {
	Token t = lexer_peek(lexer);

	if (t.type == TT_CANCELATION) {
		lexer->nextchar += 2;
	} else if (t.c != '\0') {
		lexer->nextchar += 1;
	}

	return t;
}
