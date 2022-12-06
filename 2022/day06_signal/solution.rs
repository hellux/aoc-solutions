use std::io::Read;

fn marker(s: &[u8], n: usize) -> usize {
    s.windows(n)
        .take_while(|w| {
            let mut w = w.to_vec();
            w.sort();
            w.windows(2).any(|pair| pair[0] == pair[1])
        })
        .count()
}

fn main() {
    let input = std::io::stdin()
        .bytes()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    println!("{}", marker(&input, 4));
    println!("{}", marker(&input, 14));
}
