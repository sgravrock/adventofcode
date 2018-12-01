struct Package: Equatable {
	let l: Int
	let w: Int
	let h: Int
	
	static func ==(lhs: Package, rhs: Package) -> Bool {
		return lhs.l == rhs.l && lhs.w == rhs.w && lhs.h == rhs.h
	}
}

func paperNeeded(package: Package) -> Int {
	let side1 = package.l * package.w
	let side2 = package.l * package.h
	let side3 = package.w * package.h
	let slack = min(side1, side2, side3)
	return 2 * (side1 + side2 + side3) + slack
}

func paperNeededForPackages(_ input: String) -> Int {
	let packages = parsePackages(input)
	let sizes = packages.map({paperNeeded(package: $0)})
	return sizes.reduce(0, +)
}

func parsePackages(_ input: String) -> [Package] {
	let lines = input.components(separatedBy: "\n")
	return lines.map(parsePackage)
}

func parsePackage(_ input: String) -> Package {
	let tokens = input.components(separatedBy: "x")
	let dimensions = tokens.map({Int($0)!})
	return Package(l: dimensions[0], w: dimensions[1], h: dimensions[2])
}
