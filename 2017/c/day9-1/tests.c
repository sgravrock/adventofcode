#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "lexer.h"
#include "parser.h"

typedef struct {
	const char *name;
	bool (*fn)(void);
} RegisteredTest;

#define TEST(fn) { #fn, fn }

static bool test_lexer_next(void);
static bool test_lexer_peek(void);
static bool test_parser_groups(void);
static bool test_parser_garbage(void);
static bool test_parser_unterminated_garbage(void);
static void emit_group(int score);
static void emit_failure(void);
static bool assert_equal_int(int expected, int actual, const char *context);
static bool assert_equal_char(char expected, char actual);


static int groups[10];
static size_t ngroups;
static bool parser_failed;


int main(void) {
	RegisteredTest tests[] = {
		TEST(test_lexer_next),
		TEST(test_lexer_peek),
		TEST(test_parser_groups),
		TEST(test_parser_garbage),
		TEST(test_parser_unterminated_garbage)
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

static bool test_lexer_next(void) {
	bool ok = true;
	Lexer lexer = {"<>{}!ax,"};

	Token t = lexer_next(&lexer);
	ok = assert_equal_char('<', t.c) && ok;
	ok = assert_equal_int(TT_OPEN_ANGLE, t.type, "<") && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char('>', t.c) && ok;
	ok = assert_equal_int(TT_CLOSE_ANGLE, t.type, ">") && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char('{', t.c) && ok;
	ok = assert_equal_int(TT_OPEN_BRACE, t.type, "{") && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char('}', t.c) && ok;
	ok = assert_equal_int(TT_CLOSE_BRACE, t.type, "}") && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char('a', t.c) && ok;
	ok = assert_equal_int(TT_CANCELATION, t.type, "a") && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char('x', t.c) && ok;
	ok = assert_equal_int(TT_OTHER, t.type, "x") && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char(',', t.c) && ok;
	ok = assert_equal_int(TT_COMMA, t.type, ",") && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char('\0', t.c) && ok;
	ok = assert_equal_int(TT_END, t.type, "end of input") && ok;

	return ok;
}

static bool test_lexer_peek(void) {
	bool ok = true;
	Lexer lexer = {"<>"};

	Token t = lexer_peek(&lexer);
	ok = assert_equal_int(TT_OPEN_ANGLE, t.type, "<") && ok;
	ok = assert_equal_char('<', t.c) && ok;

	t = lexer_next(&lexer);
	ok = assert_equal_char('<', t.c) && ok;

	return ok;
}

static bool test_parser_groups(void) {
	bool ok = true;
	ngroups = 0;
	parser_failed = false;

	Lexer lexer = {"{{{}}}"};
	Parser parser = { &lexer, emit_group, emit_failure };
	parse(parser);
	ok = assert_equal_int(3, ngroups, "# groups in {{{}}}") && ok;
	ok = assert_equal_int(1, groups[0], "group 0 in {{{}}}") && ok;
	ok = assert_equal_int(2, groups[1], "group 1 in {{{}}}") && ok;
	ok = assert_equal_int(3, groups[2], "group 2 in {{{}}}") && ok;

	if (parser_failed) {
		printf("Parser failed\n");
		ok = false;
	}

	return ok;
}

static bool test_parser_garbage(void) {
	bool ok = true;
	ngroups = 0;
	parser_failed = false;

	Lexer lexer = {"{<a>,<a>,<!>>}"};
	Parser parser = { &lexer, emit_group, emit_failure };
	parse(parser);

	ok = assert_equal_int(1, ngroups, "# groups in {<a>,<a>,<!>>}") && ok;
	ok = assert_equal_int(1, groups[0], "group 0 in {<a>,<a>,<!>>}") && ok;

	if (parser_failed) {
		printf("Parser failed\n");
		ok = false;
	}

	return ok;
}

static bool test_parser_unterminated_garbage(void) {
	ngroups = 0;
	parser_failed = false;

	Lexer lexer = {"{<a}"};
	Parser parser = { &lexer, emit_group, emit_failure };
	parse(parser);

	if (!parser_failed) {
		printf("Expected parser to fail but it did not\n");
		return false;
	}

	return true;
}

static void emit_group(int score) {
	if (ngroups >= sizeof groups / sizeof *groups) {
		printf("Too many groups\n");
		exit(EXIT_FAILURE);
	}

	groups[ngroups++] = score;
}

static void emit_failure(void) {
	parser_failed = true;
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
