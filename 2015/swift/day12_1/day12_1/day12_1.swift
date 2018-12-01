import Foundation

func sumNumbers(json: String) throws -> Int {
    return sumNumbers(object: try parseJson(json))
}

func parseJson(_ json: String) throws -> Any {
    let data = json.data(using: .utf8)!
    return try JSONSerialization.jsonObject(with: data, options: .allowFragments)
}

func sumNumbers(object: Any) -> Int {
    if let n = object as? Int {
        return n
    } else if let a = object as? [Any] {
        return a.map(sumNumbers).reduce(0, +)
    } else if let dict = object as? [String:Any] {
        return dict.map({ sumNumbers(object: $1) }).reduce(0, +)
    } else {
        return 0
    }
}
