#include <iostream>
using std::cin;
using std::cout;
using std::cerr;
using std::endl;
#include <set>
#include <map>
#include <utility>
using std::pair;
#include <string>
using std::string;
#include <sstream>

enum action { on, off, toggle };
typedef pair<unsigned long, unsigned long> coordinate;
typedef std::map<string, action> action_map;

pair<string, action> find_action(string line, action_map actions);

int main(void) {
	std::set<coordinate> lit;
	action_map actions;
	actions["turn on"] = on;
	actions["turn off"] = off;
	actions["toggle"] = toggle;
	string line;

	while (std::getline(cin, line)) {
		auto i = find_action(line, actions);
		std::istringstream rest(line.substr(i.first.length()));
		long x0, y0, x1, y1;
		char comma;
		string through;
		if (!(rest >> x0 >> comma >> y0 >> through >> x1 >> comma >> y1)) {
			cerr << "Read error" << endl;
			return 1;
		}

		for (long x = x0; x <= x1; x++) {
			for (long y = y0; y <= y1; y++) {
				auto key = std::make_pair(x, y);
				switch (i.second) {
					case on:
						lit.insert(key);
						break;
					case off:
						lit.erase(key);
						break;
					case toggle:
						if (lit.find(key) == lit.end()) {
							lit.insert(key);
						} else {
							lit.erase(key);
						}
				}
			}
		}
	}

	cout << lit.size() << endl;
}

pair<string, action> find_action(string line, action_map actions) {
	for (auto a : actions) {
		if (line.find(a.first) == 0) {
			return a;
		}
	}

	cerr << "No action for line: " << line << endl;
	exit(1);
}
