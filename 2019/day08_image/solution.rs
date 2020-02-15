use std::io;
use std::iter;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

type Pixel = u32;
type Row = Vec<Pixel>;
type Layer = Vec<Row>;
type Image = Vec<Layer>;

fn get_image() -> io::Result<Image> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let pixels: Vec<Pixel> = input.chars().map(|c| c.to_digit(10).unwrap()).collect();
    let rows: Vec<Row> = pixels.chunks(WIDTH).map(|r| r.to_vec()).collect();
    let layers: Image = rows.chunks(HEIGHT).map(|r| r.to_vec()).collect();

    Ok(layers)
}

fn number_of_n(layer: &Layer, n: u32) -> u32 {
    layer.iter()
         .map(|row| row.iter()
                       .fold(0, |acc, c| if *c == n { acc+1 } else { acc }))
         .sum::<u32>()
}

fn part1(mut image: Image) -> u32 {
    image.sort_unstable_by_key(|l| number_of_n(l, 0));
    let fewest_zeroes = image.first().unwrap();
    number_of_n(fewest_zeroes, 1) * number_of_n(fewest_zeroes, 2)
}

fn overlap(l1: Layer, l2: &Layer) -> Layer {
    l1.iter()
      .zip(l2)
      .map(|(r1, r2)| r1.iter()
                        .zip(r2)
                        .map(|(p1, p2)| if *p2 == 2 { *p1 } else { *p2 })
                        .collect())
      .collect()
}

fn part2(encoded: &Image) -> String {
    let background: Layer = iter::repeat(iter::repeat(0).take(WIDTH)
                                                        .collect())
                            .take(HEIGHT)
                            .collect();
    let overlapping = encoded.iter()
                             .rev()
                             .fold(background, overlap);

    let mut image: String = String::new();
    for row in overlapping {
        let row_str: String  = row.iter()
                                  .map(|p| if *p == 0 { ' ' } else { '█' })
                                  .collect();
        image.push_str(&row_str);
        image.push('\n');
    }
    image
}

fn main() -> io::Result<()> {
    let image = get_image()?;
    println!("{}", part1(image.clone()));
    println!("{}", part2(&image));
    Ok(())
}
