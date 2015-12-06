#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <openssl/md5.h>

static int match(const char *prefix, int n);
static void dump(const unsigned char *md);

int main(int argc, char **argv)
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s prefix\n", argv[0]);
		return 1;
	}

	for (int i = 0; i < INT_MAX;  i++) {
		if (match(argv[1], i)) {
			printf("%d\n", i);
			return 0;
		}
	}

	return 1;
}

static int match(const char *prefix, int n) {
	char *msg;
	asprintf(&msg, "%s%d", prefix, n);
	unsigned char md[MD5_DIGEST_LENGTH];
	MD5((const unsigned char *)msg, strlen(msg), md);
	free(msg);

	char checkbuf[7];

	for (int i = 0; i < 3; i++) {
		sprintf(checkbuf + 2*i, "%02x", (unsigned int)md[i]);
	}

	return strncmp(checkbuf, "00000", 5) == 0;
}

static void dump(const unsigned char *md) {
	for (int i = 0; i < MD5_DIGEST_LENGTH; i++) {
		//unsigned int x = md[i] << 8 | md[i+1];
		//printf("%d", x);
		printf("%02x", (unsigned int)md[i]);
	}
}
