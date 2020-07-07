use std::io;

const DIM: usize = 3;

type Vector = [i64; DIM];

#[derive(Debug, Clone)]
struct Moon {
    pos: Vector,
    vel: Vector,
}

fn get_moons() -> io::Result<Vec<Moon>> {
    let mut moons = Vec::new();

    loop {
        let mut input = String::new();
        let len = io::stdin().read_line(&mut input)?;
        if len == 0 { break };

        let filtered: String = input
            .chars()
            .filter(|c| "<>= xyz\n".find(|d| d == *c) == None)
            .collect();
        let coords: Vec<i64> = filtered.split(',').map(|s| s.parse().unwrap()).collect();
        let moon = Moon { pos: [ coords[0], coords[1], coords[2] ], vel: [0, 0, 0] };
        moons.push(moon);
    }

    Ok(moons)
}

fn step(ms: &mut Vec<Moon>) {
    let n = ms.len();
    /* apply gravity */
    for i in 0..n {
        for j in i+1..n {
            for d in 0..DIM {
                if ms[i].pos[d] < ms[j].pos[d] {
                    ms[i].vel[d] += 1;
                    ms[j].vel[d] -= 1;
                } else if ms[j].pos[d] < ms[i].pos[d] {
                    ms[j].vel[d] += 1;
                    ms[i].vel[d] -= 1;
                }
            }
        }
    }

    /* apply velocity */
    for m in ms {
        for d in 0..DIM {
            m.pos[d] += m.vel[d];
        }
    }
}

fn energy(m: &Moon) -> i64 {
    let abs_sum = |xs: Vector| xs.iter().map(|d| d.abs()).sum::<i64>();
    abs_sum(m.pos) * abs_sum(m.vel)
}

fn part1(init: &Vec<Moon>) -> i64 {
    let mut ms = init.clone();
    for _ in 0..1000 { step(&mut ms); }
    ms.iter().map(energy).sum::<i64>()
}

fn gcd(a: i64, b: i64) -> i64 {
    if a < b { gcd(a, b-a) }
    else if b < a { gcd(a-b, b) }
    else { a }
}

fn lcm(a: i64, b: i64) -> i64 {
    a * b / gcd(a, b)
}

fn part2(init: &Vec<Moon>) -> i64 {
    let n = init.len();
    let mut ms = init.clone();

    let mut steps: Vector = [0, 0, 0];

    let mut s = 0;
    loop {
        step(&mut ms);
        s += 1;

        let mut finished = true;
        for d in 0..DIM {
            if steps[d] == 0 {
                let mut eq = true;
                for i in 0..n {
                    if ms[i].pos[d] != init[i].pos[d] ||
                       ms[i].vel[d] != init[i].vel[d] {
                        eq = false;
                        break;
                    }
                }
                if eq { steps[d] = s; }
                else { finished = false; }
            }
        }

        if finished { break };
    }

    lcm(steps[0], lcm(steps[1], steps[2]))
}

fn main() -> io::Result<()> {
    let moons = get_moons()?;

    println!("{}", part1(&moons));
    println!("{}", part2(&moons));

    Ok(())
}
