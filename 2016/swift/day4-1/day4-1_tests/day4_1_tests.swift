import XCTest

class day4_1_tests: XCTestCase {
	func test_parse_input() {
		let input = [
			"bkwzkqsxq-tovvilokx-nozvyiwoxd-172[fstek]",
			"wifilzof-wbiwifuny-yhachyylcha-526[qrazx]",
		]
		let expected = [
			Room(enc_name: "bkwzkqsxqtovvilokxnozvyiwoxd", sector: 172, checksum: "fstek"),
			Room(enc_name: "wifilzofwbiwifunyyhachyylcha", sector: 526, checksum: "qrazx"),
		]
		XCTAssertEqual(expected, parse_input(input));
	}
	
	func test_is_real() {
		XCTAssertTrue(is_real(Room(enc_name: "aaaaabbbzyx", sector: 123, checksum: "abxyz")))
		XCTAssertTrue(is_real(Room(enc_name: "abcdefgh", sector: 987, checksum: "abcde")))
		XCTAssertTrue(is_real(Room(enc_name: "notarealroom", sector: 404, checksum: "oarel")))
		XCTAssertFalse(is_real(Room(enc_name: "totallyrealroom", sector: 200, checksum: "decoy")))
	}
}
