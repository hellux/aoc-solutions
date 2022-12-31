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

fn traverse(
    m: &Map,
    p: &[Instr],
    edge: &dyn Fn((isize, isize), usize) -> ((isize, isize), usize),
) -> isize {
    let (mut y, mut x) = m.keys().min().unwrap();
    let mut dir = 0;

    for i in p {
        match i {
            Instr::Walk(n) => {
                for _ in 0..*n {
                    let (dy, dx) = DIRS[dir];
                    let mut pos_next = (y + dy, x + dx);
                    let mut dir_next = dir;
                    if !m.contains_key(&pos_next) {
                        (pos_next, dir_next) = edge((y, x), dir);
                    }
                    match m[&pos_next] {
                        '.' => ((y, x), dir) = (pos_next, dir_next),
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
        (
            *if dx + dy < 0 { line.max() } else { line.min() }.unwrap(),
            dir,
        )
    })
}

fn part2(m: &Map, p: &[Instr]) -> isize {
    /* Normalized index:
     *    _
     *  _|1|___     ^
     * |2|0|4|5|    | north
     *   |3|
     */
    const R: usize = 0;
    const D: usize = 1;
    const L: usize = 2;
    const U: usize = 3;
    // adjacency list for normalized cube, left is adjacent face, right is rotation (up is normal)
    const ADJACENT: [[(usize, usize); 4]; 6] = [
        /* R       D       L       U */
        [(4, L), (3, U), (2, R), (1, D)], // 0
        [(4, U), (0, U), (2, U), (5, U)], // 1
        [(0, L), (3, L), (5, R), (1, L)], // 2
        [(4, D), (5, D), (2, D), (0, D)], // 3
        [(5, L), (3, R), (0, R), (1, R)], // 4
        [(2, L), (3, D), (4, R), (1, U)], // 5
    ];

    let l = ((m.len() / 6) as f64).sqrt() as usize;
    let li = l as isize;
    // array index is normalized index, left is index, right is rotation
    let cube = if l == 4 {
        [(0, 0), (1, 2), (2, 1), (3, 0), (5, 2), (4, 2)] // example cube
    } else {
        [(0, 0), (5, 3), (3, 2), (2, 0), (1, 0), (4, 2)] // input
    };

    let (h, w) = (
        m.keys().map(|(y, _)| y).max().unwrap(),
        m.keys().map(|(_, x)| x).max().unwrap(),
    );
    let pos_face = |(y, x)| ((y - 1) / li, (x - 1) / li);
    let faces = (1..=*h)
        .step_by(l)
        .flat_map(|y| {
            (1..*w)
                .step_by(l)
                .filter_map(move |x| m.contains_key(&(y, x)).then(|| pos_face((y, x))))
        })
        .collect::<Vec<_>>();

    traverse(m, p, &|(y, x), dir| {
        let src_face = pos_face((y, x));
        let src_face_i = faces.iter().position(|f| *f == src_face).unwrap();
        let src_face_n = cube.iter().position(|(i, _)| *i == src_face_i).unwrap();
        let (_src_face_i, rot_src) = cube[src_face_n];
        assert_eq!(_src_face_i, src_face_i);
        let dir_n = (rot_src + dir) % 4;
        let (dst_face_n, dst_side_n) = ADJACENT[src_face_n][dir_n];
        let (dst_face_i, rot_dst) = cube[dst_face_n];
        let dst_side = (dst_side_n + 4 - rot_dst) % 4;
        let ofs = (if dir % 2 == 0 { y } else { x } - 1) % li;
        let ofs = if (dir + 1) % 4 < 2 { ofs } else { li - 1 - ofs };
        let ofs = li - 1 - ofs;
        let (yb, xb) = faces[dst_face_i];
        let yb = yb * li + 1;
        let xb = xb * li + 1;
        let (yi, xi) = match dst_side {
            R => (ofs, li - 1),
            D => (li - 1, li - 1 - ofs),
            L => (li - 1 - ofs, 0),
            U => (0, ofs),
            _ => unreachable!(),
        };
        let pos_next = (yb + yi, xb + xi);
        let dir_next = (dst_side + 2) % 4;
        (pos_next, dir_next)
    })
}

fn main() {
    let (map, path) = get_input();
    println!("{}", part1(&map, &path));
    println!("{}", part2(&map, &path));
}
