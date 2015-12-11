#include <map>
#include <vector>
#include <queue>
#include <iostream>
#include <sstream>
using namespace std;

istream &readline(vector<string> &cmd);
uint16_t argval(string arg, const map<string, uint16_t> &lines);
bool maybe_run(const vector<string> &cmd, map<string, uint16_t> &lines);

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

		if (!maybe_run(command, lines)) {
			commands.push(command); // wait until its args are available
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

bool maybe_run(const vector<string> &cmd, map<string, uint16_t> &lines) {
	try {
		if (cmd[1] == "->") {
			lines[cmd[2]] = argval(cmd[0], lines);
		} else if (cmd[1] == "AND" && cmd[3] == "->") {
			lines[cmd[4]] = argval(cmd[0], lines) & argval(cmd[2], lines);
		} else if (cmd[1] == "OR" && cmd[3] == "->") {
			lines[cmd[4]] = argval(cmd[0], lines) | argval(cmd[2], lines);
		} else if (cmd[1] == "LSHIFT" && cmd[3] == "->") {
			lines[cmd[4]] = argval(cmd[0], lines) << argval(cmd[2], lines);
		} else if (cmd[1] == "RSHIFT" && cmd[3] == "->") {
			lines[cmd[4]] = argval(cmd[0], lines) >> argval(cmd[2], lines);
		} else if (cmd[0] == "NOT" && cmd[2] == "->") {
			lines[cmd[3]] = ~argval(cmd[1], lines);
		} else {
			cout << "nope" << endl;
			for (auto s: cmd) {
				cout << s << " ";
			}

			cout << endl;
			exit(1);
		}

		return true;
	} catch (const out_of_range &e) {
		return false;
	} catch (const exception &e) {
		cout << e.what() << endl;
		cout << "While processing cmd: ";
		for (auto s: cmd) {
			cout << s << " ";
		}

		cout << endl;
		exit(1);
	}
}
