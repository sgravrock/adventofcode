#ifndef LEXER_H
#define LEXER_H

typedef struct {
	const char *nextchar;
} Lexer;


typedef enum {
	TT_OPEN_BRACE,
	TT_CLOSE_BRACE,
	TT_OPEN_ANGLE,
	TT_CLOSE_ANGLE,
	TT_CANCELATION,
	TT_COMMA,
	TT_END,
	TT_OTHER
} TokenType;

typedef struct {
	TokenType type;
	char c;
} Token;

Token lexer_peek(Lexer *lexer);
Token lexer_next(Lexer *lexer);

#endif
