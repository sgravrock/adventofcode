import Foundation

func decompressed_length(_ input: String) -> Int {
	var result: Int = 0
	let scanner = Scanner(string: input)
	
	while !scanner.isAtEnd {
		var prefix: NSString?
		
		if scanner.scanUpTo("(", into: &prefix) { // also scans to end of string
			result += prefix!.length
		}
		
		if scanner.scanString("(", into: nil) {
			var nchars = -1
			scanner.scanInt(&nchars)
			scanner.scanString("x", into: nil)
			var ntimes = -1
			scanner.scanInt(&ntimes)
			scanner.scanString(")", into: nil)
			
			let toRepeat = decompressed_length(substring(of: input,
			                                             from: scanner.scanLocation,
			                                             to: scanner.scanLocation + nchars))
			scanner.scanLocation += nchars
			result += toRepeat * ntimes
		}
	}
	
	return result
}

func substring(of: String, from: Int, to: Int) -> String {
	let 💩 = of.index(of.startIndex, offsetBy: from)
	let 💩💩 = of.index(of.startIndex, offsetBy: to)
	let 💩💩💩 = 💩..<💩💩
	return of.substring(with: 💩💩💩)
}
