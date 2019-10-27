use std::io;

const WIDTH: usize = 1000;

enum InstrType {
    TOGGLE,
    ON,
    OFF,
}

struct Instr {
    instr: InstrType,
    start: (i32, i32),
    end: (i32, i32),
}

fn create_rect(instr: &Instr) -> Vec<(i32, i32)> {
    let mut rectangle: Vec<(i32, i32)> = Vec::new();
    let ((x1, y1), (x2, y2)) = (instr.start, instr.end);
    for x in x1..x2+1 {
        for y in y1..y2+1 {
            rectangle.push((x, y));
        }
    }
    rectangle
}

fn get_instructions() -> io::Result<Vec<Instr>> {
    let mut instructions: Vec<Instr> = Vec::new();
    loop {
        let mut input = String::new();
        let len = io::stdin().read_line(&mut input)?;
        if len == 0 { break };

        let words: Vec<&str> = input.split_whitespace().collect();
        let c = words.len();
        let instr = match words.get(c-4) {
            Some(&"toggle") => { InstrType::TOGGLE }
            Some(&"on")     => { InstrType::ON }
            Some(&"off")    => { InstrType::OFF }
            _               => { InstrType::ON }
        };
        let start: Vec<&str> = (&words[c-3]).split(',').collect();
        let end: Vec<&str> = (&words[c-1]).split(',').collect();

        let x1 = (&start[0]).parse().unwrap();
        let y1 = (&start[1]).parse().unwrap();
        let x2 = (&end[0]).parse().unwrap();
        let y2 = (&end[1]).parse().unwrap();

        instructions.push(Instr {
            instr: instr,
            start: (x1, y1),
            end: (x2, y2)
        })
    }
    Ok(instructions)
}

fn part1(instructions: &Vec<Instr>) -> i32 {
    let mut lights: [bool; WIDTH*WIDTH] = [false; WIDTH*WIDTH];
    for instr in instructions {
        let func: Box<Fn(bool)->bool> = match instr.instr {
            InstrType::OFF    => { Box::new(|_ppev| false) }
            InstrType::ON     => { Box::new(|_prev| true) }
            InstrType::TOGGLE => { Box::new(|prev| !prev) }
        };
        for (x, y) in create_rect(instr) {
            let i: usize = (x as usize)*WIDTH+(y as usize);
            lights[i] = func(lights[i]);
        }
    }
    lights.iter().map(|l| if *l {1} else {0}).sum()
}

fn part2(instructions: &Vec<Instr>) -> i32 {
    let mut lights: [i32; WIDTH*WIDTH] = [0; WIDTH*WIDTH];
    for instr in instructions {
        let func: Box<Fn(i32)->i32> = match instr.instr {
            InstrType::OFF    => { Box::new(|prev| std::cmp::max(0, prev-1)) }
            InstrType::ON     => { Box::new(|prev| prev+1) }
            InstrType::TOGGLE => { Box::new(|prev| prev+2) }
        };
        for (x, y) in create_rect(instr) {
            let i: usize = (x as usize)*WIDTH+(y as usize);
            lights[i] = func(lights[i]);
        }
    }
    lights.iter().sum()
}

fn main() -> io::Result<()> {
    let instructions = get_instructions()?;
    println!("part1: {}", part1(&instructions));
    println!("part2: {}", part2(&instructions));
    Ok(())
}
