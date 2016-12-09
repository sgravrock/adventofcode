import Foundation

func decompress(_ input: String) -> String {
	var result: [String] = []
	let scanner = Scanner(string: input)
	
	while !scanner.isAtEnd {
		var prefix: NSString?
		
		if scanner.scanUpTo("(", into: &prefix) { // also scans to end of string
			result.append(prefix! as String)
		}
		
		if scanner.scanString("(", into: nil) {
			var nchars = -1
			scanner.scanInt(&nchars)
			scanner.scanString("x", into: nil)
			var ntimes = -1
			scanner.scanInt(&ntimes)
			scanner.scanString(")", into: nil)
			
			let toRepeat = substring(of: input,
									 from: scanner.scanLocation,
									 to: scanner.scanLocation + nchars)
			scanner.scanLocation += nchars
			
			for _ in 1...ntimes {
				result.append(toRepeat)
			}
		}
	}
	
	return result.joined()
}

func substring(of: String, from: Int, to: Int) -> String {
	let 💩 = of.index(of.startIndex, offsetBy: from)
	let 💩💩 = of.index(of.startIndex, offsetBy: to)
	let 💩💩💩 = 💩..<💩💩
	return of.substring(with: 💩💩💩)
}
