use std::io::Read;

fn terminals(c: u8, width: isize) -> Option<[isize; 2]> {
    match c {
        b'-' => Some([-1, 1]),
        b'|' => Some([-width, width]),
        b'J' => Some([-width, -1]),
        b'7' => Some([-1, width]),
        b'L' => Some([-width, 1]),
        b'F' => Some([1, width]),
        _ => None,
    }
}

fn connects(c: u8, from: isize, to: isize, width: isize) -> bool {
    if c == b'S' {
        true
    } else if let Some([a, b]) = terminals(c, width) {
        from + a == to || from + b == to
    } else {
        false
    }
}

fn main() {
    let mut field = Vec::new();
    std::io::stdin().read_to_end(&mut field).unwrap();
    let width = field.iter().position(|c| *c == b'\n').unwrap() as isize + 1;
    let start = field.iter().position(|c| *c == b'S').unwrap() as isize;

    let mut pos = start;
    let mut prev = start;
    let mut pipe_ordered = Vec::new();
    while pos != start || prev == start {
        pipe_ordered.push(pos);
        let cur = field[pos as usize];
        for d in [-width, -1, 1, width] {
            let pos_adj = pos + d;
            if !(0 < pos_adj && pos_adj < field.len() as isize) {
                continue;
            }
            if pos_adj == prev {
                continue;
            }

            let adj = field[pos_adj as usize];
            if connects(cur, pos, pos_adj, width)
                && (pos_adj == start || connects(adj, pos_adj, pos, width))
            {
                prev = pos;
                pos = pos_adj;
                break;
            }
        }
    }

    println!("{}", pipe_ordered.len() / 2);

    let pipe: std::collections::HashSet<_> = pipe_ordered.iter().collect();
    let (mut left, mut right) = (
        std::collections::HashSet::new(),
        std::collections::HashSet::new(),
    );
    pipe_ordered
        .iter()
        .zip(pipe_ordered.iter().skip(1))
        .for_each(|(pos_prev, pos)| {
            let d = pos - pos_prev;
            let l = match d {
                -1 => width,
                1 => -width,
                _ if d == -width => -1,
                _ if d == width => 1,
                _ => unreachable!(),
            };
            let r = -l;
            let l0 = pos_prev + l;
            let l1 = pos_prev + l + d;
            let r0 = pos_prev + r;
            let r1 = pos_prev + r + d;
            if (0 < l0 && l0 < field.len() as isize) && !pipe.contains(&l0) {
                left.insert(l0);
            }
            if (0 < l1 && l1 < field.len() as isize) && !pipe.contains(&l1) {
                left.insert(l1);
            }
            if (0 < r0 && r0 < field.len() as isize) && !pipe.contains(&r0) {
                right.insert(r0);
            }
            if (0 < r1 && r1 < field.len() as isize) && !pipe.contains(&r1) {
                right.insert(r1);
            }
        });

    let mut inner = if left.len() < right.len() {
        left
    } else {
        right
    };

    let mut new = std::collections::HashSet::new();
    loop {
        for pos in &inner {
            for d in [-width, -1, 1, width] {
                let pos_adj = pos + d;
                if (0 < pos_adj && pos_adj < field.len() as isize)
                    && !pipe.contains(&pos_adj)
                    && !inner.contains(&pos_adj)
                {
                    new.insert(pos_adj);
                }
            }
        }
        if new.is_empty() {
            break;
        }
        for i in new.drain() {
            inner.insert(i);
        }
    }

    println!("{}", inner.len());
}
