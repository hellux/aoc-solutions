use std::io::Read;

#[derive(Debug)]
enum Instr {
    Walk(isize),
    Turn(char),
}

type Map = std::collections::HashMap<(isize, isize), char>;

fn get_input() -> (Map, Vec<Instr>) {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let &[m, p, ..] = input.split("\n\n").collect::<Vec<_>>().as_slice() else { panic!() };

    let map = m
        .split("\n")
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars().enumerate().filter_map(move |(x, c)| {
                matches!(c, '.' | '#').then_some(((y as isize + 1, x as isize + 1), c))
            })
        })
        .collect();

    let mut path = Vec::new();
    let mut start = 0;
    for (i, c) in p.trim().chars().enumerate() {
        if matches!(c, 'L' | 'R') {
            let n = p[start..i].parse().unwrap();
            path.push(Instr::Walk(n));
            path.push(Instr::Turn(c));
            start = i + 1;
        }
    }
    if start < p.len() {
        path.push(Instr::Walk(p[start..].trim().parse().unwrap()));
    }

    (map, path)
}

const DIRS: &[(isize, isize)] = &[(0, 1), (1, 0), (0, -1), (-1, 0)];

fn traverse(m: &Map, p: &[Instr], edge: &dyn Fn((isize, isize), usize) -> (isize, isize)) -> isize {
    let (mut y, mut x) = m.keys().min().unwrap();
    let mut dir = 0;

    for i in p {
        match i {
            Instr::Walk(n) => {
                let (dy, dx) = DIRS[dir];
                for _ in 0..*n {
                    let mut pos_next = (y + dy, x + dx);
                    if !m.contains_key(&pos_next) {
                        pos_next = edge((y, x), dir);
                    }
                    match m[&pos_next] {
                        '.' => (y, x) = pos_next,
                        '#' => break,
                        _ => unreachable!(),
                    }
                }
            }
            Instr::Turn(c) => match c {
                'L' => dir = if dir == 0 { 3 } else { dir - 1 },
                'R' => dir = (dir + 1) % 4,
                _ => unreachable!(),
            },
        }
    }

    1000 * y + 4 * x + dir as isize
}

fn part1(m: &Map, p: &[Instr]) -> isize {
    traverse(m, p, &|(y, x), dir| {
        let (dy, dx) = DIRS[dir];
        let line = m
            .keys()
            .filter(|(yy, xx)| if dx == 0 { *xx == x } else { *yy == y });
        *if dx + dy < 0 { line.max() } else { line.min() }.unwrap()
    })
}

fn main() {
    let (map, path) = get_input();
    println!("{}", part1(&map, &path));
}
