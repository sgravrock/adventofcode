import Foundation

struct Room : Equatable {
	let enc_name: String
	let sector: Int
	let checksum: String
	
	static func ==(lhs: Room, rhs: Room) -> Bool {
		return lhs.enc_name == rhs.enc_name && lhs.sector == rhs.sector && lhs.checksum == rhs.checksum
	}
}

func is_real(_ room: Room) -> Bool {
	return room.checksum == expected_checksum(room.enc_name)
}

func expected_checksum(_ enc_name: String) -> String {
	let freqs = to_frequency(enc_name)
	let sorted = freqs.map({ $0 })
		.sorted(by: { (a: (key: Character, value: Int), b: (key: Character, value: Int)) -> Bool in
			return a.value > b.value || (a.value == b.value && a.key < b.key)
		})
		.map { $0.key }
	return String([sorted[0], sorted[1], sorted[2], sorted[3], sorted[4]])
}

func to_frequency(_ name: String) -> [Character:Int] {
	var freqs: [Character:Int] = [:]
	
	for c in name.characters {
		if freqs[c] == nil {
			freqs[c] = 1
		} else {
			freqs[c]! += 1
		}
	}
	
	return freqs
}

func parse_input(_ input: [String]) -> [Room] {
	return input.map({ (line: String) -> Room in
		var tokens = line.components(separatedBy: CharacterSet(charactersIn: "-[]"))
		_ = tokens.popLast() // consume empty trailing element
		let checksum = tokens.popLast()!
		let sector = Int(tokens.popLast()!)!
		let name = tokens.joined()
		return Room(enc_name: name, sector: sector, checksum: checksum)
	})
}
