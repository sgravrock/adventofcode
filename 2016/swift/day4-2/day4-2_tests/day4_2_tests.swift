import XCTest

class day4_2_tests: XCTestCase {
	func test_parse_input() {
		let input = [
			"bkwzkqsxq-tovvilokx-nozvyiwoxd-172[fstek]",
			"wifilzof-wbiwifuny-yhachyylcha-526[qrazx]",
			]
		let expected = [
			Room(enc_name: "bkwzkqsxq-tovvilokx-nozvyiwoxd", sector: 172, checksum: "fstek"),
			Room(enc_name: "wifilzof-wbiwifuny-yhachyylcha", sector: 526, checksum: "qrazx"),
			]
		XCTAssertEqual(expected, parse_input(input));
	}
	
	func test_is_real() {
		XCTAssertTrue(is_real(Room(enc_name: "aaaaa-bbbzyx", sector: 123, checksum: "abxyz")))
		XCTAssertTrue(is_real(Room(enc_name: "abcdefgh", sector: 987, checksum: "abcde")))
		XCTAssertTrue(is_real(Room(enc_name: "notarealroom", sector: 404, checksum: "oarel")))
		XCTAssertFalse(is_real(Room(enc_name: "totallyrealroom", sector: 200, checksum: "decoy")))
	}
	
	func test_decrypt() {
		XCTAssertEqual("VERY ENCRYPTED NAME", decrypt(Room(enc_name: "qzmt-zixmtkozy-ivhz", sector: 343, checksum: "")))
	}
	
	func test_rotate() {
		let input: UnicodeScalar = "Z"
		XCTAssertEqual("E", rotate(input, times:343))
	}
}
