use std::io;

const DIM: usize = 3;

#[derive(Debug)]
struct Moon {
    pos: [i64; DIM],
    vel: [i64; DIM],
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
    let potential = m.pos.iter().map(|d| d.abs()).sum::<i64>();
    let kinetic = m.vel.iter().map(|d| d.abs()).sum::<i64>();
    potential * kinetic
}

fn part1(mut ms: Vec<Moon>) -> i64 {
    for _ in 0..1000 { step(&mut ms); }
    ms.iter().map(energy).sum::<i64>()
}

fn main() -> io::Result<()> {
    let moons = get_moons()?;

    println!("{}", part1(moons));

    Ok(())
}
