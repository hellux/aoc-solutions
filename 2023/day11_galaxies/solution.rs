use std::io::Read;

fn main() {
    let mut cosmos = Vec::new();
    std::io::stdin().read_to_end(&mut cosmos).unwrap();
    let cosmos = cosmos
        .split(|c| *c == b'\n')
        .filter(|r| !r.is_empty())
        .collect::<Vec<_>>();
    let width = cosmos[0].len();
    let height = cosmos.len();

    let empty_rows = cosmos
        .iter()
        .enumerate()
        .filter(|(_, row)| !row.contains(&b'#'))
        .map(|(y, _)| y)
        .collect::<std::collections::HashSet<_>>();

    let empty_cols = (0..width)
        .filter_map(|x| {
            (0..height)
                .map(|y| cosmos[y][x])
                .find(|c| *c == b'#')
                .is_none()
                .then_some(x)
        })
        .collect::<std::collections::HashSet<_>>();

    let galaxies = cosmos
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .filter_map(move |(x, c)| (*c == b'#').then_some((y, x)))
        })
        .collect::<Vec<_>>();

    let mut part1 = 0;
    let mut gaps = 0;
    for (i, (ay, ax)) in galaxies.iter().enumerate() {
        for (by, bx) in &galaxies[i + 1..] {
            for i in 0..isize::abs(*ay as isize - *by as isize) {
                if empty_rows.contains(&(i as usize + usize::min(*ay, *by))) {
                    gaps += 1;
                } else {
                    part1 += 1;
                }
            }
            for i in 0..isize::abs(*ax as isize - *bx as isize) {
                if empty_cols.contains(&(i as usize + usize::min(*ax, *bx))) {
                    gaps += 1;
                } else {
                    part1 += 1;
                }
            }
        }
    }

    println!("{}", part1);

    let gap_p = 6usize;
    let gap = 10usize.pow(gap_p as u32);
    gaps += part1 / gap;
    part1 = part1 % gap;
    println!("{}{:0>gap_p$}", gaps, part1);
}
