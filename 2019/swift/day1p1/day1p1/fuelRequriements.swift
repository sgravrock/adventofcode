import Cocoa

func fuelRequirements(_ input: String) -> Int {
	return input
		.split(separator: "\n")
		.map({ Int(String($0))! })
		.map(fuelRequirement)
		.reduce(0, +)
}

func fuelRequirement(forMass mass: Int) -> Int {
	return Int(floor(Double(mass) / 3.0)) - 2
}
