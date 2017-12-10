#include "lexer.h"
#include <stdlib.h>

struct lexer {
	const char *nextchar;
};

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

Token lexer_next(Lexer *lexer) {
	Token t;
	t.c = *(lexer->nextchar);

	if (t.c) {
		lexer->nextchar++;
	}

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
			t.type = TT_BANG;
			break;
		case '\0':
			t.type = TT_END;
			break;
		default:
			t.type = TT_OTHER;
	}

	return t;
}
