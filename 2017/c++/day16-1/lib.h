#ifndef LIB_H
#define LIB_H

#include <vector>
#include <deque>
#include <string>
#include <memory>

class Instruction {
public:
	virtual void perform(std::deque<char> &programs) = 0;
};

class Spin: public Instruction {
public:
	size_t size;

	explicit Spin(size_t size);
	virtual void perform(std::deque<char> &programs);
};

class Exchange: public Instruction {
public:
	size_t a;
	size_t b;

	Exchange(size_t a, size_t b);
	virtual void perform(std::deque<char> &programs);
};

class Partner: public Instruction {
public:
	char a;
	char b;

	Partner(char a, char b);
	virtual void perform(std::deque<char> &programs);
};


void dance(std::deque<char> &programs, std::string instructions);
std::vector<std::string> tokenize(std::string instructions);
std::vector<std::shared_ptr<Instruction>> parse(std::string instructions);


#endif
