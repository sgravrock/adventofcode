import Foundation

let packages = [
	1,
	2,
	3,
	5,
	7,
	13,
	17,
	19,
	23,
	29,
	31,
	37,
	41,
	43,
	53,
	59,
	61,
	67,
	71,
	73,
	79,
	83,
	89,
	97,
	101,
	103,
	107,
	109,
	113
]
print("Start: \(NSDate())")
print(idealConfigurations(packages: packages).first!.quantumEntanglement)
print("End: \(NSDate())")
