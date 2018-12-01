CXXFLAGS+= --std=c++14
LDFLAGS+= -lstdc++

day16_1: day16_1.o lib.o
tests: tests.o lib.o
tests.o: tests.cc lib.h
lib.o: lib.cc lib.h
day16_1.o: day16_1.cc lib.h
