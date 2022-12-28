unit Config;

interface

	type
		DebugLogLevelType = (debugNone, debugSome, debugPainfullyVerbose);

	const
		debugLogLevel = debugSome;
	{ 300 in puzzle input, plus two dividers }
		maxNumPackets = 302;
	{ 21263 bytes for puzzle input plus a bit for dividers, rounded up }
		maxBufSize = 22000;

implementation

end.