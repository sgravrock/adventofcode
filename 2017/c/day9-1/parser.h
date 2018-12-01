#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

typedef struct {
	Lexer *lexer;
	void (*emit_group)(int score);
	void (*fail)(void);
} Parser;

void parse(Parser parser);

#endif
