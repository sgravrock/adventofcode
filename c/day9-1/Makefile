CFLAGS+= -Wall -W --std=c99 -pedantic -O2

all: day9-1 run-tests

day9-1: day9-1.o lexer.o

run-tests: tests
	./tests

tests: tests.o lexer.o
