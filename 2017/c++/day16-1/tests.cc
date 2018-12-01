#define CATCH_CONFIG_MAIN
#include "../catch.hpp"
#include "lib.h"

using std::vector;
using std::deque;
using std::string;
using std::shared_ptr;

TEST_CASE("tokenize") {
	vector<string> expected = {"x11/4", "pd/h", "x10/5"};
	REQUIRE(tokenize("x11/4,pd/h,x10/5") == expected);
}

TEST_CASE("parse") {
	vector<shared_ptr<Instruction>> expected = {
		shared_ptr<Instruction>(new Spin(1)),
		shared_ptr<Instruction>(new Spin(1)),
		shared_ptr<Instruction>(new Partner('d', 'h')),
		shared_ptr<Instruction>(new Exchange(10, 5))
	};
	auto actual = parse("s1,pd/h,x10/5");
	auto spin = dynamic_cast<Spin *>(actual[0].get());
	REQUIRE(spin != 0);
	REQUIRE(spin->size == 1);
	auto partner = dynamic_cast<Partner *>(actual[1].get());
	REQUIRE(partner != 0);
	REQUIRE(partner->a == 'd');
	REQUIRE(partner->b == 'h');
	auto exchange = dynamic_cast<Exchange *>(actual[2].get());
	REQUIRE(exchange != 0);
	REQUIRE(exchange->a == 10);
	REQUIRE(exchange->b == 5);
}

TEST_CASE("sample dance") {
	deque<char> programs = {'a', 'b', 'c', 'd', 'e'};
	dance(programs, "s1,x3/4,pe/b");
	vector<char> expected = {'b', 'a', 'e', 'd', 'c'};
	vector<char> actual;
	copy(programs.begin(), programs.end(), back_inserter(actual));
	REQUIRE(actual == expected);
}
