#include <stdio.h>
#include "lib.h"

static int value1(size_t n_inserts, size_t steps_per);

int main(void) {
	printf("%d\n", value1(50000000, 370));
	//printf("%d\n", value1(2017, 3));
	return 0;
}

static int value1(size_t n_inserts, size_t steps_per) {
	size_t pos = 0;
	int result = -1;

	for (size_t i = 1; i <= n_inserts; i++) {
		pos = ((pos + steps_per) % i) + 1;

		if (pos == 1) {
			result = i;
		}
	}

	return result;
}
