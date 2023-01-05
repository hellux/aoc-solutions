use std::io::Read;

const ORE: usize = 0;
const CLAY: usize = 1;
const OBSIDIAN: usize = 2;
const GEODE: usize = 3;
const N: usize = 4;
const FLEET_INIT: [T; N] = [1, 0, 0, 0];
const STOCK_INIT: [T; N] = [0, 0, 0, 0];
type T = u16;

type Blueprint = [[T; N - 1]; N];

fn get_input() -> Vec<Blueprint> {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    input
        .split("\nB")
        .map(|bp| {
            let mut ws = bp.split_whitespace();
            let mut nth = |i| ws.nth(i).unwrap().parse().unwrap();
            [
                [nth(6), 0, 0],      // ore robot
                [nth(5), 0, 0],      // clay robot
                [nth(5), nth(2), 0], // obsidian robot
                [nth(5), 0, nth(2)], // geode robot
            ]
        })
        .collect::<Vec<_>>()
}

fn dfs(mins: T, fleet: [T; N], mut stock: [T; N], bp: &Blueprint, best: &mut T) {
    let mins = mins - 1;
    let can_buy =
        [ORE, CLAY, OBSIDIAN, GEODE].map(|bot| (0..N - 1).all(|i| stock[i] >= bp[bot][i] as T));
    (0..N).for_each(|i| stock[i] += fleet[i]);
    let potential = stock[GEODE] + fleet[GEODE] * mins + mins * (mins + 1) / 2;
    if mins == 0 {
        let g = stock[GEODE];
        if g > *best {
            *best = g;
        }
    } else if potential > *best {
        (0..N)
            .filter(|bot| can_buy[*bot])
            .filter(|bot| {
                (*bot == GEODE) || fleet[*bot] < *bp.map(|bp| bp[*bot]).iter().max().unwrap()
            })
            .filter(|bot| *bot != ORE || mins > bp[ORE][ORE])
            .for_each(|bot| {
                let mut fleet = fleet;
                fleet[bot] += 1;
                let mut stock = stock;
                (0..N - 1).for_each(|i| stock[i] -= bp[bot][i] as T);
                dfs(mins, fleet, stock, bp, best)
            });
        if !can_buy[GEODE] && !can_buy.iter().all(|i| *i) {
            dfs(mins, fleet, stock, bp, best);
        }
    }
}

fn part1(bps: &[Blueprint]) -> T {
    bps.iter()
        .map(|bp| {
            let mut best = 0;
            dfs(24, FLEET_INIT, STOCK_INIT, bp, &mut best);
            best
        })
        .enumerate()
        .map(|(i, geodes)| (i as T + 1) * geodes)
        .sum()
}

fn main() {
    let blueprints = get_input();
    println!("{}", part1(&blueprints));
}
