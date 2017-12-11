CFLAGS+= -Wall -W --std=c99 -pedantic -O2

all: day9-2 run-tests

clean:
	rm -f *.o tests day9-1

day9-2: day9-2.o lexer.o parser.o

run-tests: tests
	./tests

tests: tests.o lexer.o parser.o
