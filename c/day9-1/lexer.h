#ifndef LEXER_H
#define LEXER_H

typedef struct lexer Lexer;

typedef enum {
	TT_OPEN_BRACE,
	TT_CLOSE_BRACE,
	TT_OPEN_ANGLE,
	TT_CLOSE_ANGLE,
	TT_BANG,
	TT_END,
	TT_OTHER
} TokenType;

typedef struct {
	TokenType type;
	char c;
} Token;

Lexer *lexer_make(const char *input);
void lexer_destroy(Lexer *lexer);
Token lexer_next(Lexer *lexer);

#endif
