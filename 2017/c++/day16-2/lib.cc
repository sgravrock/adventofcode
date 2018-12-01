#include "lib.h"

#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <cstdio>

using std::deque;
using std::vector;
using std::string;
using std::replace;
using std::istringstream;
using std::istream_iterator;
using std::shared_ptr;
using std::transform;
using std::back_inserter;
using std::runtime_error;
using std::sscanf;
using std::swap;

namespace {
	shared_ptr<Instruction> parseOne(string token) {
		int ai, bi;
		char ac, bc;

		if (sscanf(token.c_str(), "s%d", &ai) == 1) {
			return shared_ptr<Instruction>(new Spin(ai));
		}

		if (sscanf(token.c_str(), "x%d/%d", &ai, &bi) == 2) {
			return shared_ptr<Instruction>(new Exchange(ai, bi));
		}

		if (sscanf(token.c_str(), "p%c/%c", &ac, &bc) == 2) {
			return shared_ptr<Instruction>(new Partner(ac, bc));
		}

		throw runtime_error("Can't parse token: " + token);
	}

	template <class Iter, class T>
	Iter must_find(Iter first, Iter last, const T& val) {
		auto p = find(first, last, val);

		if (p == last) {
			throw runtime_error("Can't find");
		}

		return p;
	}
}

deque<char> dance(deque<char> programs, vector<shared_ptr<Instruction>> instructions) {
	for (const shared_ptr<Instruction>& ip : instructions) {
		ip->perform(programs);
	}

	return programs;
}

int period(deque<char> programs, vector<shared_ptr<Instruction>> instructions) {
	auto original = programs;

	for (int i = 0;; i++) {
		programs = dance(programs, instructions);

		if (programs == original) {
			return i + 1;
		}
	}
}


vector<string> tokenize(string instructions) {
	replace(instructions.begin(), instructions.end(), ',', ' ');
	istringstream iss(instructions);
	vector<string> tokens {
		istream_iterator<string>{iss},
		istream_iterator<string>{}
	};
	return tokens;
}

vector<shared_ptr<Instruction>> parse(string instructions) {
	auto tokens = tokenize(instructions);
	vector<shared_ptr<Instruction>> result;
	transform(tokens.begin(), tokens.end(), back_inserter(result), parseOne);
	return result;
}



Spin::Spin(size_t size): size(size) {}

void Spin::perform(deque<char> &programs) {
	for (size_t i = 0; i < size; i++) {
		programs.push_front(programs.back());
		programs.pop_back();
	}
}

Partner::Partner(char a, char b): a(a), b(b) {}

void Partner::perform(deque<char> &programs) {
	auto ap = must_find(programs.begin(), programs.end(), a);
	auto bp = must_find(programs.begin(), programs.end(), b);
	swap(*ap, *bp);
}

Exchange::Exchange(size_t a, size_t b): a(a), b(b) {}

void Exchange::perform(deque<char> &programs) {
	swap(programs[a], programs[b]);
}
