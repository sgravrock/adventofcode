#include <map>
#include <vector>
#include <queue>
#include <iostream>
#include <sstream>
using namespace std;

istream &readline(vector<string> &cmd);
bool maybe_run(const vector<string> &cmd);
uint16_t argval(string arg, const map<string, uint16_t> &lines);

int main(void) {
	vector<string> command;
	queue<vector<string> > commands;

	while (readline(command)) {
		commands.push(command);
	}

	map<string, uint16_t> lines;

	// Assumes no dependency cycles. Won't terminate if there are any.
	while (!commands.empty()) {
		command = commands.front();
		commands.pop();

		try {
			if (command[1] == "->") {
				lines[command[2]] = argval(command[0], lines);
			} else if (command[1] == "AND" && command[3] == "->") {
				lines[command[4]] = argval(command[0], lines) & argval(command[2], lines);
			} else if (command[1] == "OR" && command[3] == "->") {
				lines[command[4]] = argval(command[0], lines) | argval(command[2], lines);
			} else if (command[1] == "LSHIFT" && command[3] == "->") {
				lines[command[4]] = argval(command[0], lines) << argval(command[2], lines);
			} else if (command[1] == "RSHIFT" && command[3] == "->") {
				lines[command[4]] = argval(command[0], lines) >> argval(command[2], lines);
			} else if (command[0] == "NOT" && command[2] == "->") {
				lines[command[3]] = ~argval(command[1], lines);
			} else {
				cout << "nope" << endl;
				for (auto s: command) {
					cout << s << " ";
				}
	
				cout << endl;
				return 1;
			}
		} catch (const out_of_range &e) {
			commands.push(command); // wait until its args are available
		} catch (const exception &e) {
			cout << e.what() << endl;
			cout << "While processing command: ";
			for (auto s: command) {
				cout << s << " ";
			}
	
			cout << endl;
			return 1;
		}
	}

	for (auto i: lines) {
		cout << i.first << ": " << i.second << endl;
	}
}

istream &readline(vector<string> &tokens) {
	string line;

	if (!getline(cin, line)) {
		return cin;
	}

	tokens.clear();
	string token;
	istringstream s(line);

	while (s >> token) {
		tokens.push_back(token);
	}

	return cin;
}

uint16_t argval(string arg, const map<string, uint16_t> &lines) {
	try {
		return stoi(arg);
	} catch (const invalid_argument &e) {
		return lines.at(arg);
	}
}
