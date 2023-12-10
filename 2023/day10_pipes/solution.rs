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
    let mut n = 0;
    while pos != start || prev == start {
        n += 1;
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

    println!("{}", n / 2);
}
