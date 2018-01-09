CXXFLAGS+= --std=c++14
LDFLAGS+= -lstdc++

day16_2: day16_2.o lib.o
tests: tests.o lib.o
tests.o: tests.cc lib.h
lib.o: lib.cc lib.h
day16_2.o: day16_2.cc lib.h
