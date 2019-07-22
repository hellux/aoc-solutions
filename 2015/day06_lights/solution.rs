use std::io;

const WIDTH: usize = 1000;

fn main() -> io::Result<()> {
    let mut lights: [bool; WIDTH*WIDTH] = [false; WIDTH*WIDTH];

    loop {
        let mut input = String::new();
        let len = io::stdin().read_line(&mut input)?;
        if len == 0 { break };

        let words: Vec<&str> = input.split_whitespace().collect();
        let c = words.len();
        let instr = words.get(c-4);
        let start: Vec<&str> = words.get(c-3).unwrap().split(',').collect();
        let end: Vec<&str> = words.get(c-1).unwrap().split(',').collect();

        let x1: usize = start.get(0).unwrap().parse().unwrap();
        let y1: usize = start.get(1).unwrap().parse().unwrap();
        let x2: usize = end.get(0).unwrap().parse().unwrap();
        let y2: usize = end.get(1).unwrap().parse().unwrap();

        let mut rectangle: Vec<(usize,usize)> = Vec::new();
        for x in x1..x2+1 {
            for y in y1..y2+1 {
                rectangle.push((x, y));
            }
        }

        match instr {
            Some(&"toggle") => {
                for (x, y) in rectangle {
                    let i: usize = x*WIDTH+y;
                    lights[i] = match lights[i] {
                        true => { false }
                        false => { true }
                    }

                }
            }
            Some(&"on") => {
                for (x, y) in rectangle {
                    let i: usize = x*WIDTH+y;
                    lights[i] = true;
                }
            }
            Some(&"off") => {
                for (x, y) in rectangle {
                    let i: usize = x*WIDTH+y;
                    lights[i] = false;
                }
            }
            _ => {}
        }
    }

    let mut lit: i32 = 0;
    for l in lights.iter() {
        if *l {
            lit += 1;
        }
    }
    println!("part1: {}", lit);

    Ok(())
}
