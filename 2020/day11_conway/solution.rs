use self::Square::*;

#[derive(Copy, Clone, Debug, PartialEq)]
enum Square {
    Floor,
    Empty,
    Occupied,
}

#[derive(Clone, Debug)]
struct Layout {
    width: usize,
    height: usize,
    squares: Vec<Square>,
}

impl Layout {
    fn new() -> Self {
        let mut squares = Vec::new();
        let mut width = 0;
        let mut height = 0;

        loop {
            let mut line = String::new();
            if std::io::stdin().read_line(&mut line).unwrap() == 0 {
                break;
            }
            let mut row: Vec<_> = line
                .chars()
                .filter_map(|c| match c {
                    '.' => Some(Floor),
                    'L' => Some(Empty),
                    '#' => Some(Occupied),
                    _ => None,
                })
                .collect();

            width = row.len();
            height += 1;
            squares.append(&mut row);
        }

        Layout {
            width,
            height,
            squares,
        }
    }

    fn tick(&mut self, p2: bool) -> bool {
        let n = self.width * self.height;
        let tol = if p2 { 5 } else { 4 };

        let ds = [
            (-1, -1),
            (0, -1),
            (1, -1),
            (-1, 0),
            (1, 0),
            (-1, 1),
            (0, 1),
            (1, 1),
        ];
        let mut vis = vec![0; n];
        for y in 0..self.height {
            for x in 0..self.width {
                for (dx, dy) in &ds {
                    for i in 1.. {
                        let nx = x as i16 + dx * i;
                        let ny = y as i16 + dy * i;
                        let inside = 0 <= nx
                            && nx < self.width as i16
                            && 0 <= ny
                            && ny < self.height as i16;

                        if !inside {
                            break;
                        } else {
                            match self.squares
                                [ny as usize * self.width + nx as usize]
                            {
                                Occupied => {
                                    vis[y * self.width + x] += 1;
                                    break;
                                }
                                Empty => break,
                                Floor => {}
                            }
                        }

                        if !p2 {
                            break;
                        }
                    }
                }
            }
        }

        let mut changed = false;
        for i in 0..n {
            let visible = vis[i];
            match self.squares[i] {
                Empty => {
                    if visible == 0 {
                        self.squares[i] = Occupied;
                        changed = true;
                    }
                }
                Occupied => {
                    if visible >= tol {
                        self.squares[i] = Empty;
                        changed = true;
                    }
                }
                Floor => {}
            };
        }

        changed
    }

    fn count(&self, sq: Square) -> usize {
        self.squares.iter().filter(|s| s == &&sq).count()
    }
}

/*
impl Layout {
    fn get(&self, x: i16, y: i16) -> Option<Square> {
        let index = y * self.width as i16 + x;
        if index >= 0 {
            self.squares.get(index as usize).map(|s| *s)
        } else {
            None
        }
    }
}

use std::fmt;

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Floor => ".",
                Empty => "L",
                Occupied => "#",
            }
        )
    }
}

impl fmt::Display for Layout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                write!(f, "{}", self.get(x as i16, y as i16).unwrap())?;
            }
            write!(f, "\n")?;
        }

        Ok(())
    }
}
*/

fn part1(mut layout: Layout) -> usize {
    while layout.tick(false) {}
    layout.count(Occupied)
}

fn part2(mut layout: Layout) -> usize {
    while layout.tick(true) {}
    layout.count(Occupied)
}

fn main() {
    let layout = Layout::new();

    println!("{}", part1(layout.clone()));
    println!("{}", part2(layout.clone()));
}
