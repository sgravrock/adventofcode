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
	size_t n;
	Claim *v;
} ClaimVec;

typedef struct {
	int x;
	int y;
} Coord;

Coord get_bounds(ClaimVec claims);
int overlap_area(ClaimVec claims, Coord bounds);
ClaimVec xparse_claims(FILE *fp);
void *xmalloc(size_t size);
void *xrealloc(void *p, size_t size);

#define max(a, b) ((a) > (b) ? (a) : (b))

int main(void) {
	ClaimVec claims = xparse_claims(stdin);
	Coord bounds = get_bounds(claims);
	printf("%d\n", overlap_area(claims, bounds));
	free(claims.v);
	return 0;
}

Coord get_bounds(ClaimVec claims) {
	Coord result = {0, 0};

	for (size_t i = 0; i < claims.n; i++) {
		result.x = max(result.x, claims.v[i].left + claims.v[i].width - 1);
		result.y = max(result.y, claims.v[i].top + claims.v[i].height - 1);
	}

	return result;
}

int overlap_area(ClaimVec claims, Coord bounds) {
	int counts[bounds.x + 1][bounds.y + 1];
	int result = 0;

	for (int x = 0; x <= bounds.x; x++) {
		for (int y = 0; y <= bounds.y; y++) {
			counts[x][y] = 0;
		}
	}


	for (size_t i = 0; i < claims.n; i++) {
		Claim claim = claims.v[i];

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

ClaimVec xparse_claims(FILE *fp) {
	size_t capacity = 16;
	ClaimVec result = {0, xmalloc(capacity * sizeof *result.v)};
	Claim *nc = &result.v[0];

	while (fscanf(fp, "#%*d @ %d,%d: %dx%d\n",
			&nc->left, &nc->top, &nc->width, &nc->height) == 4) {

		if (++result.n >= capacity) {
			capacity *= 2;
			result.v = xrealloc(result.v, capacity * sizeof *result.v);
		}

		nc = &result.v[result.n];
	}

	if (!feof(fp)) {
		char buf[80];
		fgets(buf, sizeof buf, fp);
		char *nl = strchr(buf, '\n');
		if (nl) *nl = '\0';
		fprintf(stderr, "Parse error before '%s'\n", buf);
		exit(EXIT_FAILURE);
	}

	return result;
}

void *xmalloc(size_t size) {
	void *result = malloc(size);

	if (!result) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}

	return result;
}

void *xrealloc(void *p, size_t size) {
	void *result = realloc(p, size);

	if (!result) {
		perror("realloc");
		exit(EXIT_FAILURE);
	}

	return result;
}
