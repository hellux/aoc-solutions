use std::collections::HashMap;

fn part1(starting: &[u64]) -> u64 {
    let mut spoken: HashMap<u64, usize> = starting
        .iter()
        .take(starting.len() - 1) // skip last one
        .enumerate()
        .map(|(i, n)| (*n, i + 1)) // map numbers to last turn it was used
        .collect();

    let mut prev = *starting.last().unwrap();
    for i in (starting.len())..2020 {
        let next = spoken.get(&prev).map(|last| i - last).unwrap_or(0) as u64;
        spoken.insert(prev, i);
        prev = next;
    }

    prev
}

fn main() {
    let mut line = String::new();
    std::io::stdin().read_line(&mut line).unwrap();
    let starting_numbers: Vec<u64> = line
        .replace("\n", "")
        .split(",")
        .map(|c| c.parse().unwrap())
        .collect();

    println!("{}", part1(&starting_numbers));
}
