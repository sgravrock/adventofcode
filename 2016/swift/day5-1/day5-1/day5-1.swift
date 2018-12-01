import Foundation

typealias NextCharResult = (i: Int, c: Character);

func findPassword(doorId: String) -> String {
	var chars: [Character] = []
	var i = 0
	
	while chars.count < 8 {
		let result = next_character(doorId: doorId, start: i)
		chars.append(result.c)
		i = result.i + 1
	}
	
	return String(chars)
}

func next_character(doorId: String, start: Int) -> NextCharResult {
	var i = start
	
	while true {
		let r = autoreleasepool(invoking: { () -> NextCharResult? in
			let hash = md5("\(doorId)\(i)")
		
			if isValid(hash: hash) {
				let s = toHexString(bytes: hash)
				return (i: i, c: s[s.index(s.startIndex, offsetBy: 5)])
			}
			
			return nil
		})
		
		if r != nil {
			return r!
		}
		
		i += 1
	}
}

func md5(_ value: String) -> [UInt8] {
	let data = value.data(using: String.Encoding.utf8)!
	return data.withUnsafeBytes { (bytes: UnsafePointer<UInt8>) in
		var binDigest = [UInt8](repeating: 0, count: Int(CC_MD5_DIGEST_LENGTH))
		CC_MD5(bytes, CC_LONG(data.count), &binDigest)
		return binDigest
	}
}

func toHexString(bytes: [UInt8]) -> String {
	return bytes.map { String(format: "%02x", $0) }.joined(separator: "")
}

func isValid(hash: [UInt8]) -> Bool {
	return hash[0] == 0 && hash[1] == 0 && hash[2] < 0x10;
}
