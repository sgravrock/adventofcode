mod input;

fn main() {
	let decoded = decode_image(parse_image(input::puzzle_input(), 25, 6));
	println!("{}", format_layer(decoded, 25));
}

type Layer = Vec<i32>;
type Image = Vec<Layer>;

fn parse_image(input: &str, width: usize, height: usize) -> Image {
	let digits = input
		.chars()
		.map(|c| c.to_digit(10).unwrap() as i32)
		.collect::<Vec<i32>>();

	digits
		.chunks_exact(width * height)
		.map(|s| s.to_vec())
		.collect()
}

#[test]
fn test_parse_image() {
	let input = "123456789012";
	let expected = vec![
		vec![1,2,3,4,5,6],
		vec![7,8,9,0,1,2]
	];
	assert_eq!(parse_image(input, 3, 2), expected);
}

fn decode_image(image: Image) -> Layer {
	let layer_size = image[0].len();
	(0..layer_size)
		.map(|i| decode_pixel(&image, i))
		.collect()
}

fn decode_pixel(image: &Image, i: usize) -> i32 {
	image.iter()
		.filter_map(|layer| {
			if layer[i] == 2 { None } else { Some(layer[i]) }
		})
		.next()
		.unwrap_or(2)
}

#[test]
fn test_decode_image() {
	let input = parse_image("0222112222120000", 2, 2);
	let expected = vec![0,1,1,0];
	assert_eq!(decode_image(input), expected);
}

fn format_layer(layer: Layer, width: usize) -> String {
	let mut result = String::new();

	for row in layer.chunks_exact(width) {
		for pixel in row {
			result.push_str(if *pixel == 1 { "*" } else { " " });
		}

		result.push_str("\n");
	}

	result
}

#[test]
fn test_format_layer() {
	let layer = vec![0,1,1,0];
	let expected = " *\n* \n";
	assert_eq!(format_layer(layer, 2), expected);
}
