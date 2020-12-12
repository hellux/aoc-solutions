use std::fmt;

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

    fn get(&self, x: i16, y: i16) -> Option<Square> {
        let index = y * self.width as i16 + x;
        if index >= 0 {
            self.squares.get(index as usize).map(|s| *s)
        } else {
            None
        }
    }

    fn tick(&mut self) -> bool {
        let n = self.width * self.height;

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
        let mut adjs = vec![0; n];
        for y in 0..self.height {
            for x in 0..self.width {
                if self.squares[y * self.width + x] == Occupied {
                    for (dx, dy) in &ds {
                        let nx = x as i16 + dx;
                        let ny = y as i16 + dy;
                        if 0 <= nx
                            && nx < self.width as i16
                            && 0 <= ny
                            && ny < self.height as i16
                        {
                            adjs[ny as usize * self.width + nx as usize] += 1;
                        }
                    }
                }
            }
        }

        let mut changed = false;
        for i in 0..n {
            let adjacent = adjs[i];
            match self.squares[i] {
                Empty => {
                    if adjacent == 0 {
                        self.squares[i] = Occupied;
                        changed = true;
                    }
                }
                Occupied => {
                    if adjacent >= 4 {
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

fn part1(mut layout: Layout) -> usize {
    while layout.tick() {}
    layout.count(Occupied)
}

fn main() {
    let layout = Layout::new();

    println!("{}", part1(layout.clone()));
}
