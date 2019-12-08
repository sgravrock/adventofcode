mod input;

fn main() {
	let decoded = decode_image(parse_image(input::puzzle_input(), 25, 6));

	for row in decoded {
		for pixel in row {
			print!("{}", if pixel == 1 { '*' } else { ' ' });
		}
		println!("");
	}
}

type Layer = Vec<Vec<i32>>;
type Image = Vec<Layer>;

fn parse_image(input: &str, width: usize, height: usize) -> Image {
	let digits: Vec<i32> = input
		.chars()
		.map(|c| c.to_digit(10).unwrap() as i32)
		.collect();
	let layer_size = width * height;
	let mut layers: Image = vec![];


	for i in 0..digits.len() {
		let li = i / layer_size;
		let ri = (i % layer_size) / width;
		if i % layer_size == 0 { layers.push(vec![]); }
		if i % width == 0 { layers[li].push(vec![]); }
		layers[li][ri].push(digits[i]);
	}

	layers
}

#[test]
fn test_parse_image() {
	let input = "123456789012";
	let expected = vec![
		vec![
			vec![1,2,3],
			vec![4,5,6]
		],
		vec![
			vec![7,8,9],
			vec![0,1,2]
		]
	];
	assert_eq!(parse_image(input, 3, 2), expected);
}

fn decode_image(image: Image) -> Layer {
	let mut result: Layer = vec![];

	for y in 0..image[0].len() {
		let mut row = vec![];

		for x in 0..image[0][0].len() {
			row.push(decode_pixel(&image, y, x));
		}

		result.push(row);
	}

	result
}

fn decode_pixel(image: &Image, y: usize, x: usize) -> i32 {
	image.iter()
		.filter_map(|layer| {
			if layer[y][x] == 2 { None } else { Some(layer[y][x]) }
		})
		.next()
		.unwrap_or(2)
}

#[test]
fn test_decode_image() {
	let input = parse_image("0222112222120000", 2, 2);
	let expected = vec![
		vec![0, 1],
		vec![1, 0]
	];
	assert_eq!(decode_image(input), expected);
}
