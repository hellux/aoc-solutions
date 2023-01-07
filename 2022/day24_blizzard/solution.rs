use std::io::Read;

type Blizzards<'a> = (&'a [Vec<char>], isize, isize);

fn closest_path(
    m: isize,
    start: (isize, isize),
    end: (isize, isize),
    (bs, w, h): Blizzards,
) -> isize {
    let (mut branches, mut next) = (
        std::collections::HashSet::new(),
        std::collections::HashSet::new(),
    ); // use set to avoid duplicating same pos, exploding no. of branches

    for m in m.. {
        if branches.is_empty() {
            branches.insert(start);
        }
        (branches, next) = (next, branches);
        for (x, y) in next.drain() {
            for (dx, dy) in [(1, 0), (0, 1), (0, 0), (0, -1), (-1, 0)] {
                let (x, y) = (x + dx, y + dy);
                if (x, y) == end {
                    return m;
                } else if 0 <= x && x < w && 0 <= y && y < h {
                    let free = bs[y as usize][((x - m).rem_euclid(w)) as usize] != '>'
                        && bs[((y - m).rem_euclid(h)) as usize][x as usize] != 'v'
                        && bs[y as usize][((x + m) % w) as usize] != '<'
                        && bs[((y + m) % h) as usize][x as usize] != '^';
                    if free {
                        branches.insert((x, y));
                    }
                }
            }
        }
    }

    panic!()
}

fn part1(blizzards: Blizzards) -> isize {
    let (_, w, h) = blizzards;
    closest_path(0, (0, -1), (w - 1, h), blizzards)
}

fn part2(blizzards: Blizzards) -> isize {
    let (_, w, h) = blizzards;
    let start = (0, -1);
    let end = (w - 1, h);
    let m = 0;
    let m = closest_path(m, start, end, blizzards);
    let m = closest_path(m, end, start, blizzards);
    let m = closest_path(m, start, end, blizzards);
    m
}

fn main() {
    let blizzards = {
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).unwrap();

        input
            .split('\n')
            .filter(|l| !l.is_empty())
            .filter(|l| l.chars().filter(|c| *c == '#').count() == 2)
            .map(|row| row.chars().filter(|c| *c != '#').collect::<Vec<_>>())
            .collect::<Vec<_>>()
    };
    let w = blizzards[0].len() as isize;
    let h = blizzards.len() as isize;
    let blizzards = (blizzards.as_slice(), w, h);

    println!("{}", part1(blizzards));
    println!("{}", part2(blizzards));
}
