use std::convert::TryFrom;
use std::io::Read;

type Sprite = [u8; 4];

const SPRITES: [Sprite; 5] = [
    [0b1111, 0b0000, 0b0000, 0b0000],
    [0b0010, 0b0111, 0b0010, 0b0000],
    [0b0111, 0b0100, 0b0100, 0b0000],
    [0b0001, 0b0001, 0b0001, 0b0001],
    [0b0011, 0b0011, 0b0000, 0b0000],
];

const W: u8 = 7;

struct Sim<'a> {
    jet_pattern: &'a [bool],
    chamber: Vec<u8>,
    rock: usize,
    tick: usize,
    x: isize,
    y: isize,
}

impl<'a> Sim<'a> {
    fn new(jet_pattern: &'a [bool]) -> Self {
        Self {
            jet_pattern,
            chamber: Vec::new(),
            rock: 0,
            tick: 0,
            x: 2,
            y: 3,
        }
    }

    fn tick(&mut self) -> bool {
        // push
        let push_left = self.jet_pattern[self.tick % self.jet_pattern.len()];
        let dx = if push_left { -1 } else { 1 };
        if self.can_move(dx, 0) {
            self.x += dx;
        }

        let fall = self.can_move(0, -1);
        if fall {
            self.y -= 1;
        } else {
            // land
            for (y, sr) in self.sprite().iter().enumerate() {
                let ny = self.y as usize + y;
                while self.chamber.len() <= ny {
                    self.chamber.push(1 << W);
                }
                self.chamber[ny] |= sr << self.x;
            }

            // respawn
            self.rock += 1;
            self.x = 2;
            self.y = self.height() as isize + 3;
        }

        self.tick += 1;

        !fall
    }

    fn height(&self) -> usize {
        self.chamber
            .iter()
            .rposition(|row| *row & ((1 << W) - 1) > 0)
            .map(|i| i + 1)
            .unwrap_or(0)
    }

    fn can_move(&self, dx: isize, dy: isize) -> bool {
        if let (Ok(nx), Ok(ny)) = (usize::try_from(self.x + dx), usize::try_from(self.y + dy)) {
            for (y, sr) in self.sprite().iter().enumerate() {
                let cr = self.chamber.get(ny + y).copied().unwrap_or(1 << W);
                if cr & (sr << nx) > 0 {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    fn sprite(&self) -> Sprite {
        SPRITES[self.rock % SPRITES.len()]
    }

    fn _draw(&self) {
        for y in (0..self.height().max(self.y as usize + 4)).rev() {
            let row = self.chamber.get(y).copied().unwrap_or(0);
            print!("{:>2}|", y);
            for x in 0..W {
                print!(
                    "{}",
                    if row & (1 << x) > 0 {
                        '#'
                    } else if usize::try_from(y as isize - self.y).map_or(false, |y| self
                        .sprite()
                        .get(y)
                        .map_or(false, |row| row & (1 << (x as isize - self.x)) > 0))
                    {
                        '@'
                    } else {
                        '.'
                    }
                )
            }
            println!("|");
        }
        println!("  +-------+");
        println!("   0123456");
    }
}

fn part1(jet_pattern: &[bool]) -> usize {
    let mut sim = Sim::new(jet_pattern);
    while sim.rock < 2022 {
        sim.tick();
    }
    sim.height()
}

fn part2(jet_pattern: &[bool]) -> usize {
    const END: usize = 1_000_000_000_000;
    let mut sim = Sim::new(jet_pattern);

    let mut height_prev = 0;
    let mut diffs = Vec::new();
    let mut found_cycle = false;
    let mut skip_height = 0;
    let mut skip_rocks = 0;

    while sim.rock + skip_rocks < END {
        let landed = sim.tick();

        if landed && !found_cycle {
            let height = sim.height();
            let diff = height - height_prev;
            height_prev = height;
            diffs.push(diff);
            let dl = diffs.len();
            for l in 10..dl / 2 {
                let cycle = (0..l).all(|i| diffs[dl - l + i] == diffs[dl - 2 * l + i]);
                if cycle {
                    found_cycle = true;
                    let skip_cycles = (END - sim.tick) / l;
                    let cycle_diff = diffs[dl - l..].iter().sum::<usize>();
                    skip_height = skip_cycles * cycle_diff;
                    skip_rocks = skip_cycles * l;
                    break;
                }
            }
        }
    }

    sim.height() + skip_height
}

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let jet_pattern = input
        .chars()
        .filter(|c| matches!(c, '<' | '>'))
        .map(|c| c == '<')
        .collect::<Vec<_>>();

    println!("{}", part1(&jet_pattern));
    println!("{}", part2(&jet_pattern));
}
