#include <stdio.h>

int main(void) {
	const int target = 36000000;

	for (int houseNum = 1; ; houseNum++) {
		int numPresents = 0;

		for (int i = 1; i <= houseNum; i++) {
			if (houseNum % i == 0) {
				numPresents += i * 10;
			}
		}

		if (numPresents >= target) {
			printf("%d\n", houseNum);
			return 0;
		}
	}
}
