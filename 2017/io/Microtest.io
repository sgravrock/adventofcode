Microtest := Object clone

Microtest init := method(
	self numAssertions := 0
	self numFailed := 0
)

Microtest summarize := method(
	self writeln("")

	if(numFailed == 0,
		self writeln("All " .. self numAssertions .. " tests passed."),
		self writeln(self numFailed .. " of " .. self numAssertions .. " tests failed.")
		self die
	)
)

Microtest die := method(
	System exit
)

Microtest assert := method(ok, name,
	self recordResult(ok)
	msg := if(ok, "PASS: ", "FAIL: ") .. name
	self writeln(msg)
)

Microtest assertEqual := method(a, b, name,
	self recordResult(a == b)
	if(a == b,
		self writeln("PASS: " .. name),
		self writeln("FAIL: " .. name)
		self writeln("   " .. a .. " != " .. b)
	)
)

Microtest recordResult := method(ok,
	self numAssertions = self numAssertions + 1
	if(ok != true, self numFailed = self numFailed + 1)
)
