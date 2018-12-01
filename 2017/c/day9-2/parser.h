#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"

typedef struct {
	Lexer *lexer;
	void (*emit_garbage_char)(void);
	void (*fail)(void);
} Parser;

void parse(Parser parser);

#endif
