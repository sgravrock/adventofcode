mod input;

fn main() {
	println!("{}", image_score(parse_image(input::puzzle_input(), 25, 6)));
}

type Layer = Vec<Vec<u32>>;
type Image = Vec<Layer>;

fn parse_image(input: &str, width: usize, height: usize) -> Image {
	let digits: Vec<u32> = input
		.chars()
		.map(|c| c.to_digit(10).unwrap())
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

fn image_score(image: Image) -> usize {
	let target_layer = image.iter()
		.min_by_key(|layer| num_of(layer, 0))
		.unwrap();

	num_of(target_layer, 1) * num_of(target_layer, 2)
}

fn num_of(layer: &Layer, digit: u32) -> usize {
	layer.iter()
		.flatten()
		.filter(|d| **d == digit)
		.count()
}


#[test]
fn test_image_score() {
	let image = vec![
		vec![
			vec![1,2,2],
			vec![1,2,6],
			vec![0,3,3]  // higher score but fewer 0 digits
		],
		vec![
			vec![7,8,9],
			vec![1,1,2],
			vec![0,0,3]
		]
	];

	assert_eq!(image_score(image), 6);
}
