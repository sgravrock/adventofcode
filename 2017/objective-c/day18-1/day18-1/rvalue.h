typedef struct Rvalue {
	BOOL isRef;
	union {
		long long value;
		char ref;
	} refOrValue;
} Rvalue;
