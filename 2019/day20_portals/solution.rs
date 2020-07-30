use std::io;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::BinaryHeap;

type Maze = Vec<Vec<char>>;
type Pos = (i64, i64);
type Name = [char; 2];
type Portals = HashMap<Pos, Portal>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Portal {
    dest: Pos,
    level: i64,
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct DijkstraNode {
    cost: usize,
    pos: Pos,
    level: i64,
}

impl Ord for DijkstraNode {
    fn cmp(&self, other: &DijkstraNode) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for DijkstraNode {
    fn partial_cmp(&self, other: &DijkstraNode) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

const PASSAGE: char = '.';
const ADJACENT: [Pos; 4] = [ (1, 0), (0, 1), (-1, 0), (0, -1) ];
const START_NAME: Name = ['A', 'A'];
const END_NAME: Name = ['Z', 'Z'];

fn get_maze() -> io::Result<Maze> {
    let mut maze: Maze = Vec::new();
    loop {
        let mut line = String::new();
        let len = io::stdin().read_line(&mut line)?;
        if len == 0 { break };

        maze.push(Vec::from(line.chars().collect::<Vec<char>>()));
    }

    Ok(maze)
}

fn parse_nodes(maze: &Maze) -> io::Result<(Portals, Pos, Pos)> {
    let mut previous: HashMap<Name, Portal> = HashMap::new();
    let mut portals: Portals  = HashMap::new();
    let mut start: Pos = (-1, -1);
    let mut end: Pos = (-1, -1);

    let height = maze.len() as i64;
    let width = maze[0].len() as i64;

    for y in 0..height-1 {
        for x in 0..width-1 {
            if maze[y as usize][x as usize] == PASSAGE {
                for (dx, dy) in &ADJACENT {
                    let n1 = maze[(y+dy) as usize][(x+dx) as usize];

                    if n1.is_uppercase() {
                        let n2 = maze[(y+2*dy) as usize][(x+2*dx) as usize];
                        let name = if *dx > 0 || *dy > 0 { [n1, n2] }
                                   else { [n2, n1] };

                        let is_outer = x == 2 || x == width-4 ||
                                       y == 2 || y == height-3;

                        let p1 = Portal {
                            dest: (x, y),
                            level: if is_outer {1} else {-1},
                        };
                        if name == START_NAME {
                            start = p1.dest;
                        } else if name == END_NAME {
                            end = p1.dest;
                        } else if let Some(p2) = previous.get(&name) {
                            portals.insert(p1.dest, p2.clone());
                            portals.insert(p2.dest, p1);
                        } else {
                            previous.insert(name, p1);
                        }

                        break;
                    }
                }
            }
        }
    }

    Ok((portals, start, end))
}

fn dijkstra(start: Pos, end: Pos, maze: &Maze,
            portals: &Portals, ignore_level: bool) -> Option<usize> {
    let mut distances: HashMap<(Pos, i64), usize> = HashMap::new();
    let mut unvisited = BinaryHeap::new();

    unvisited.push(DijkstraNode { cost: 0, pos: start, level: 0 });
    
    while let Some(DijkstraNode { cost, pos, level }) = unvisited.pop() {
        if pos == end && level == 0 { return Some(cost); }

        for (dx, dy) in &ADJACENT {
            let (nx, ny) = (pos.0+dx, pos.1+dy);

            if maze[ny as usize][nx as usize] == PASSAGE {
                let mut neigh = DijkstraNode
                    { cost: cost + 1, pos: (nx, ny), level: level };
                if let Some(tp) = portals.get(&(nx, ny)) {
                    let nlevel = if ignore_level {0} else {level + tp.level};
                    if nlevel >= 0 {
                        neigh = DijkstraNode {
                            cost: cost + 2,
                            pos: tp.dest,
                            level: nlevel,
                        };
                    }
                }

                let nkey = (neigh.pos, neigh.level);
                let ncost = if let Some(c) = distances.get(&nkey)
                            {*c} else {usize::max_value()};
                if neigh.cost < ncost {
                    distances.insert(nkey, neigh.cost);
                    unvisited.push(neigh);
                }
            }
        }
    }

    None
}

fn part1(entry: Pos, exit: Pos, maze: &Maze, portals: &Portals) -> usize {
    dijkstra(entry, exit, maze, portals, true).unwrap()
}

fn part2(entry: Pos, exit: Pos, maze: &Maze, portals: &Portals) -> usize {
    dijkstra(entry, exit, maze, portals, false).unwrap()
}

fn main() -> io::Result<()> {
    let maze = get_maze()?;
    let (portals, entry, exit) = parse_nodes(&maze)?;

    println!("{:?}", part1(entry, exit, &maze, &portals));
    println!("{:?}", part2(entry, exit, &maze, &portals));
    
    Ok(())
}
