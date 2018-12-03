#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
	int left;
	int top;
	int width;
	int height;
} Claim;

typedef struct {
	int x;
	int y;
} Coord;

Coord get_bounds(FILE *fp);
int overlap_area(FILE *fp, Coord bounds);
bool xparse_claim(FILE *fp, Claim *claim);

#define max(a, b) ((a) > (b) ? (a) : (b))

int main(int argc, const char **argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input-file\n", argv[0] ? argv[0] : "day3p1");
		return EXIT_FAILURE;
	}

	FILE *fp = fopen(argv[1], "r");
	if (!fp) {
		perror(argv[1]);
		return EXIT_FAILURE;
	}

	Coord bounds = get_bounds(fp);
	rewind(fp);

	printf("%d\n", overlap_area(fp, bounds));

	fclose(fp);
	return 0;
}

Coord get_bounds(FILE *fp) {
	Coord result = {0, 0};
	Claim claim;

	while (xparse_claim(fp, &claim)) {
		result.x = max(result.x, claim.left + claim.width - 1);
		result.y = max(result.y, claim.top + claim.height - 1);
	}

	return result;
}

int overlap_area(FILE *fp, Coord bounds) {
	int counts[bounds.x + 1][bounds.y + 1];
	int result = 0;

	for (int x = 0; x <= bounds.x; x++) {
		for (int y = 0; y <= bounds.y; y++) {
			counts[x][y] = 0;
		}
	}

	Claim claim;

	while (xparse_claim(fp, &claim)) {
		for (int x = claim.left; x < claim.left + claim.width; x++) {
			for (int y = claim.top; y < claim.top + claim.height; y++) {
				if (counts[x][y]++ == 1) {
					result++;
				}
			}
		}
	}

	return result;
}

bool xparse_claim(FILE *fp, Claim *claim) {
	if (fscanf(fp, "#%*d @ %d,%d: %dx%d\n",
			&claim->left, &claim->top, &claim->width, &claim->height)
			== 4) {
		return true;
	} else if (feof(fp)) {
		return false;
	} else {
		char buf[80];
		fgets(buf, sizeof buf, fp);
		char *nl = strchr(buf, '\n');
		if (nl) *nl = '\0';
		fprintf(stderr, "Parse error before '%s'\n", buf);
		exit(EXIT_FAILURE);
	}
}
