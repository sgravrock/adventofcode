#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "lexer.h"

typedef struct {
	const char *name;
	bool (*fn)();
} RegisteredTest;

#define TEST(fn) { #fn, fn }

static bool test_lexer();
static bool assert_equal_int(int expected, int actual, const char *context);
static bool assert_equal_char(char expected, char actual);


int main(void) {
	RegisteredTest tests[] = {
		TEST(test_lexer)
	};

	bool ok = true;

	for (size_t i = 0; i < sizeof tests / sizeof *tests; i++) {
		if (tests[i].fn()) {
			printf("%s passed\n", tests[i].name);
		} else {
			printf("%s failed\n", tests[i].name);
			ok = false;
		}
	}

	if (ok) {
		printf("\nAll tests passed.\n");
		return EXIT_SUCCESS;
	} else {
		printf("\nSome tests failed.\n");
		return EXIT_FAILURE;
	}
}

static bool test_lexer() {
	bool ok = true;
	Lexer *lexer = lexer_make("<>{}!x");

	Token t = lexer_next(lexer);
	ok = assert_equal_char('<', t.c) && ok;
	ok = assert_equal_int(TT_OPEN_ANGLE, t.type, "<") && ok;

	t = lexer_next(lexer);
	ok = assert_equal_char('>', t.c) && ok;
	ok = assert_equal_int(TT_CLOSE_ANGLE, t.type, ">") && ok;

	t = lexer_next(lexer);
	ok = assert_equal_char('{', t.c) && ok;
	ok = assert_equal_int(TT_OPEN_BRACE, t.type, "{") && ok;

	t = lexer_next(lexer);
	ok = assert_equal_char('}', t.c) && ok;
	ok = assert_equal_int(TT_CLOSE_BRACE, t.type, "}") && ok;

	t = lexer_next(lexer);
	ok = assert_equal_char('!', t.c) && ok;
	ok = assert_equal_int(TT_BANG, t.type, "!") && ok;

	t = lexer_next(lexer);
	ok = assert_equal_char('x', t.c) && ok;
	ok = assert_equal_int(TT_OTHER, t.type, "x") && ok;

	t = lexer_next(lexer);
	ok = assert_equal_char('\0', t.c) && ok;
	ok = assert_equal_int(TT_END, t.type, "end of input") && ok;

	lexer_destroy(lexer);
	return ok;
}

static bool assert_equal_int(int expected, int actual, const char *context) {
	if (expected == actual) {
		return true;
	}

	printf("Expected %d to equal %d (%s)\n", actual, expected, context);
	return false;
}

static bool assert_equal_char(char expected, char actual) {
	if (expected == actual) {
		return true;
	}

	printf("Expected %c to equal %c\n", actual, expected);
	return false;
}
