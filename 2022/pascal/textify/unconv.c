#include <stdio.h>

// Converts \r to \n.
// This is mainly useful for text files that tr(1) can't process.
// In other cases, tr will be faster.
int main(void) {
	int c;

	while ((c = getchar()) != EOF) {
		putchar(c == '\r' ? '\n' : c);
	}

	return 0;
}
