#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
	int id;
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
Claim *find_non_overlapping(ClaimVec claims);
bool overlaps(Claim a, Claim b);
ClaimVec xparse_claims(FILE *fp);
void *xmalloc(size_t size);
void *xrealloc(void *p, size_t size);

#define max(a, b) ((a) > (b) ? (a) : (b))

int main(void) {
	ClaimVec claims = xparse_claims(stdin);
	Claim *result = find_non_overlapping(claims);

	if (result) {
		printf("%d\n", result->id);
	} else {
		printf("No result found.\n");
	}

	free(claims.v);
	return 0;
}

Claim *find_non_overlapping(ClaimVec claims) {
	bool excluded[claims.n];

	for (size_t i = 0; i < claims.n; i++) {
		excluded[i] = false;
	}

	for (size_t i = 0; i < claims.n; i++) {
		for (size_t j = i + 1; j < claims.n; j++) {
			if (overlaps(claims.v[i], claims.v[j])) {
				excluded[i] = excluded[j] = true;
			}
		}
	}

	for (size_t i = 0; i < claims.n; i++) {
		if (!excluded[i]) {
			return &claims.v[i];
		}
	}

	return NULL;
}

bool overlaps(Claim a, Claim b) {
	int aRight = a.left + a.width - 1;
	int bRight = b.left + b.width - 1;
	int aBottom = a.top + a.height - 1;
	int bBottom = b.top + b.height - 1;

	return !(a.left > bRight || b.left > aRight) &&
		!(a.top > bBottom || b.top > aBottom);
}

ClaimVec xparse_claims(FILE *fp) {
	size_t capacity = 16;
	ClaimVec result = {0, xmalloc(capacity * sizeof *result.v)};
	Claim *nc = &result.v[0];

	while (fscanf(fp, "#%d @ %d,%d: %dx%d\n",
			&nc->id, &nc->left, &nc->top, &nc->width, &nc->height) == 5) {

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
