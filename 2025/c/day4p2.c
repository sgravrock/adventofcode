#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Width and height are the same.
// If they aren't, you get to keep both pieces.
#define MAX_SZ 150

static int read_input(char grid[][MAX_SZ]);
static int solve(char grid[][MAX_SZ], int size);
static int remove_accessible(char grid[][MAX_SZ], int size);
static int accessible(char grid[][MAX_SZ], int size, int i, int j);
static int is_roll(char grid[][MAX_SZ], int size, int i, int j);
static void dump(char grid[][MAX_SZ], int size);

int main(void) {
	char grid[MAX_SZ][MAX_SZ];
	int size = read_input(grid);
	dump(grid, size);
	int result = solve(grid, size);
	printf("%d\n", result);
	return 0;
}


static int read_input(char grid[][MAX_SZ]) {
	int i = 0;
	char line[MAX_SZ + 1];

	while (i < MAX_SZ) {
		if (fgets(line, sizeof line, stdin) == NULL) {
			if (feof(stdin)) {
				break;
			} else {
				perror("read from stdin");
				exit(EXIT_FAILURE);
			}
		}

		char *p = strchr(line, '\n');
		if (!p) {
			fprintf(stderr, "Line %d (0-based) is too long\n", i);
			fprintf(stderr, "[%s]\n", line);
			exit(EXIT_FAILURE);
		}

		*p = '\0';
		strcpy(grid[i], line);

		i++;
	}

	return i;
}

static int solve(char grid[][MAX_SZ], int size) {
	int done = 0;
	int result = 0;

	while (!done) {
		int subresult = remove_accessible(grid, size);
		dump(grid, size);
		result += subresult;

		if (subresult == 0) {
			done = 1;
		} else {
			dump(grid, size);

			for (int i = 0; i < size; i++) {
				for (int j = 0; j < size; j++) {
					if (grid[i][j] == 'x') {
						grid[i][j] = '.';
					}
				}
			}
		}
	}

	return result;
}

static int remove_accessible(char grid[][MAX_SZ], int size) {
	int n = 0;

	for (int i = 0; i <= size; i++) {
		for (int j = 0; j <= size; j++) {
			if (grid[i][j] == '@' && accessible(grid, size, i, j)) {
				grid[i][j] = 'x';
				n++;
			}
		}
	}

	printf("removed %d in this pass\n", n);
	return n;
}

static int accessible(char grid[][MAX_SZ], int size, int i, int j) {
	int neighbors[][2] = {
		{-1, -1}, {-1, 0}, {-1, 1},
		{0, -1},           {0, 1},
		{1, -1},  {1, 0},  {1, 1},
	};
	int nrolls = 0;

	for (size_t k = 0; k < sizeof neighbors / sizeof *neighbors; k++) {
		if (is_roll(grid, size, i + neighbors[k][0], j + neighbors[k][1])) {
			nrolls++;
		}
	}

	return nrolls < 4;
}

static int is_roll(char grid[][MAX_SZ], int size, int i, int j) {
	if (i < 0 || i >= size || j < 0 || j >= size) {
		return 0;
	}

	return grid[i][j] != '.';
}

static void dump(char grid[][MAX_SZ], int size) {
	for (int i = 0; i < size; i++) {
		printf("%s\n", grid[i]);
	}

	puts("");
}
