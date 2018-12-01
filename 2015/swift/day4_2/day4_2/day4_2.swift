import Foundation

func mine(key: String) -> Int {
	for i in 0...Int.max {
		let hash = makeHash(key: key, n: i)
		
		if isValid(hash: hash) {
			return i
		}
	}
	
	return -1
}

func makeHash(key: String, n: Int) -> [UInt8] {
	return md5("\(key)\(n)")
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
	return bytes.map { String(format: "%02x", $0) }.joined(separator: "");
}

func isValid(hash: [UInt8]) -> Bool {
	return hash[0] == 0 && hash[1] == 0 && hash[2] == 0;
}
