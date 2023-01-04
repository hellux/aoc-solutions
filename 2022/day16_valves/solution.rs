use std::cell::RefCell;
use std::convert::TryInto;
use std::io::Read;

fn get_input() -> (Vec<(usize, Vec<usize>)>, usize) {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let names: Vec<&str> = input
        .split('\n')
        .flat_map(|l| l.split_whitespace().nth(1))
        .collect();

    let valves = input
        .split('\n')
        .flat_map(|l| {
            let [l, r]: [&str; 2] = l.split(';').collect::<Vec<_>>().try_into().ok()?;
            let rate = l.split('=').nth(1).unwrap().parse().unwrap();
            let destinatinons = r
                .split_whitespace()
                .skip(4)
                .map(|v| names.iter().position(|vn| *vn == &v[..2]).unwrap())
                .collect();
            Some((rate, destinatinons))
        })
        .collect();
    let start = names.iter().position(|v| *v == "AA").unwrap();

    (valves, start)
}

fn nearest_path(start: usize, end: usize, valves: &[(usize, Vec<usize>)]) -> usize {
    let mut unvisited = valves.iter().map(|_| true).collect::<Vec<_>>();
    let mut dist = valves.iter().map(|_| usize::MAX).collect::<Vec<_>>();
    dist[start] = 0;

    while unvisited[end] {
        let closest = (0..valves.len())
            .filter(|i| unvisited[*i])
            .min_by_key(|i| dist[*i])
            .unwrap();

        for n in &valves[closest].1 {
            let d = dist[closest] + 1;
            dist[*n] = dist[*n].min(d);
        }

        unvisited[closest] = false;
    }

    dist[end]
}

fn max_pressure(
    pos: usize,
    rounds: usize,
    open: &RefCell<&mut [bool]>,
    valves: &[(usize, Vec<usize>)],
    distances: &[Vec<usize>],
) -> usize {
    if rounds == 0 {
        return 0;
    }

    (0..valves.len())
        .filter(|i| !open.borrow()[*i])
        .map(|dst| {
            let r = (distances[pos][dst] + 1).min(rounds);
            let remaining = rounds - r;
            let provided_pressure = valves[dst].0 * remaining;

            open.borrow_mut()[dst] = true;
            let p = provided_pressure + max_pressure(dst, remaining, open, valves, distances);
            open.borrow_mut()[dst] = false;

            p
        })
        .max()
        .unwrap_or(0)
}

fn part1(valves: &[(usize, Vec<usize>)], start: usize, distances: &[Vec<usize>]) -> usize {
    let mut open = valves.iter().map(|(r, _)| *r == 0).collect::<Vec<_>>();
    max_pressure(start, 30, &RefCell::new(&mut open), valves, distances)
}

fn part2(valves: &[(usize, Vec<usize>)], start: usize, distances: &[Vec<usize>]) -> usize {
    const R: usize = 26;
    const D: usize = 2;

    let n = valves.len();
    let nonzero = (0..n).filter(|i| valves[*i].0 > 0).collect::<Vec<_>>();
    let nz = nonzero.len();
    assert!(nz <= 16);
    let lb = nz / 2 - D;
    let ub = nz / 2 + D;

    let mut max = 0;
    let mut open0 = vec![true; n];
    let mut open1 = vec![true; n];
    for mask in 0..(1u16 << nz) {
        let n1 = mask.count_ones() as usize;
        if lb < n1 && n1 < ub {
            for b in 0..nz {
                let a = mask & (1 << b) > 0;
                let i = nonzero[b];
                open0[i] = a;
                open1[i] = !a;
            }
            let p0 = max_pressure(start, R, &RefCell::new(&mut open0), valves, distances);
            let p1 = max_pressure(start, R, &RefCell::new(&mut open1), valves, distances);
            max = max.max(p0 + p1);
        }
    }
    max
}

fn main() {
    let (valves, start) = get_input();
    let distances = (0..valves.len())
        .map(|start| {
            (0..valves.len())
                .map(|end| nearest_path(start, end, &valves))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    println!("{}", part1(&valves, start, &distances));
    println!("{}", part2(&valves, start, &distances));
}
