#include <iostream>
using std::cin;
using std::cout;
using std::cerr;
using std::endl;
#include <map>
#include <utility>
using std::pair;
#include <string>
using std::string;
#include <sstream>

enum action { on, off, toggle };
typedef pair<unsigned long, unsigned long> coordinate;
typedef std::map<string, action> action_map;
typedef std::map<coordinate, int> light_map;

pair<string, action> find_action(string line, const action_map &actions);
long sum(const light_map &lights);

int main(void) {
	light_map lights;
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
				int val;
				switch (i.second) {
					case on:
						lights[key]++;
						break;
					case off:
						val = lights[key];
						lights[key] = val > 0 ? val - 1 : val;
						break;
					case toggle:
						lights[key] += 2;
				}
			}
		}
	}

	cout << sum(lights) << endl;
}

pair<string, action> find_action(string line, const action_map &actions) {
	for (auto a : actions) {
		if (line.find(a.first) == 0) {
			return a;
		}
	}

	cerr << "No action for line: " << line << endl;
	exit(1);
}

long sum(const light_map &lights) {
	long result = 0;

	for (auto i : lights) {
		result += i.second;
	}

	return result;
}
