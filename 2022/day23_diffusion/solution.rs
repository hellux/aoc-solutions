use std::io::Read;

fn _draw(elves: &[(isize, isize)]) {
    let ((xmin, ymin), (xmax, ymax)) = bbox(&elves);
    for y in ymin..=ymax {
        for x in xmin..=xmax {
            let c = if elves.iter().any(|(xx, yy)| *xx == x && *yy == y) {
                '#'
            } else {
                '.'
            };
            print!("{}", c);
        }
        println!();
    }
    println!();
}

fn sim(elves: &mut [(isize, isize)], proposals: &mut Vec<(usize, (isize, isize))>, r: usize) {
    const DIRS: [((isize, isize), [(isize, isize); 3]); 4] = [
        ((00, -1), [(-1, -1), (00, -1), (01, -1)]), // north
        ((00, 01), [(-1, 01), (00, 01), (01, 01)]), // south
        ((-1, 00), [(-1, -1), (-1, 00), (-1, 01)]), // west
        ((01, 00), [(01, -1), (01, 00), (01, 01)]), // east
    ];

    proposals.clear();

    for (i, (x, y)) in elves.iter().enumerate() {
        let completely_empty = DIRS.iter().all(|(_, ps)| {
            !ps.iter()
                .any(|(dx, dy)| elves.iter().any(|(x2, y2)| x + dx == *x2 && y + dy == *y2))
        });
        if !completely_empty {
            for d in 0..4 {
                let ((dx, dy), ps) = DIRS[(r + d) % 4];
                let dir_empty = ps
                    .iter()
                    .all(|(dx, dy)| !elves.iter().any(|(x2, y2)| x + dx == *x2 && y + dy == *y2));
                if dir_empty {
                    proposals.push((i, (x + dx, y + dy)));
                    break;
                }
            }
        }
    }

    for (i, pos) in proposals.iter() {
        if proposals.iter().filter(|(_, p)| p == pos).count() == 1 {
            elves[*i] = *pos;
        }
    }
}

fn bbox(elves: &[(isize, isize)]) -> ((isize, isize), (isize, isize)) {
    (
        (
            *elves.iter().map(|(x, _)| x).min().unwrap(),
            *elves.iter().map(|(_, y)| y).min().unwrap(),
        ),
        (
            *elves.iter().map(|(x, _)| x).max().unwrap(),
            *elves.iter().map(|(_, y)| y).max().unwrap(),
        ),
    )
}

fn part1(elves: &[(isize, isize)]) -> isize {
    let mut elves = elves.to_vec();
    let n = elves.len();
    let mut proposals = Vec::with_capacity(n);
    for r in 0..10 {
        sim(&mut elves, &mut proposals, r);
    }
    let ((xmin, ymin), (xmax, ymax)) = bbox(&elves);
    (xmax - xmin + 1) * (ymax - ymin + 1) - n as isize
}

fn main() {
    let elves = {
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).unwrap();
        input
            .split("\n")
            .enumerate()
            .flat_map(|(y, row)| {
                row.chars()
                    .enumerate()
                    .filter_map(move |(x, c)| (c == '#').then_some((x as isize, y as isize)))
            })
            .collect::<Vec<_>>()
    };
    println!("{}", part1(&elves));
}
