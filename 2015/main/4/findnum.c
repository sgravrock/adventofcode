#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <openssl/md5.h>

static int match(const char *prefix, int suffix, int nzeros);
static int has_leading_zeros(const unsigned char *md, int nzeros);

int main(int argc, char **argv)
{
	int nzeros;

	if (argc != 3 || sscanf(argv[2], "%u", &nzeros) != 1) {
		fprintf(stderr, "Usage: %s prefix nzeros\n", argv[0]);
		return 1;
	}

	for (int i = 0; i < INT_MAX;  i++) {
		if (match(argv[1], i, nzeros)) {
			printf("%d\n", i);
			return 0;
		}
	}

	return 1;
}

static int match(const char *prefix, int suffix, int nzeros) {
	char *msg;
	asprintf(&msg, "%s%d", prefix, suffix);
	unsigned char md[MD5_DIGEST_LENGTH];
	MD5((const unsigned char *)msg, strlen(msg), md);
	free(msg);
	return has_leading_zeros(md, nzeros);
}

static int has_leading_zeros(const unsigned char *md, int nzeros) {
	char buf[MD5_DIGEST_LENGTH * 2 + 1] = {0};

	for (int i = 0; i < MD5_DIGEST_LENGTH; i++) {
		sprintf(buf + 2*i, "%02x", (unsigned int)md[i]);
	}

	for (int i = 0; i < nzeros; i++) {
		if (buf[i] != '0') {
			return 0;
		}
	}

	return 1;
}
