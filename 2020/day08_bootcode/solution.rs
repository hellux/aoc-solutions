use self::InstrKind::*;

#[derive(Debug, Clone, Copy, PartialEq)]
enum InstrKind {
    Nop,
    Acc,
    Jmp,
}
#[derive(Debug, Clone, Copy)]
struct Instr {
    kind: InstrKind,
    arg: i64,
}
type Program = Vec<Instr>;

fn read_program() -> Program {
    let mut prog = Vec::new();

    loop {
        let mut line = String::new();
        let len = std::io::stdin().read_line(&mut line).unwrap();
        if len == 0 {
            break;
        };
        let words: Vec<&str> = line.split_whitespace().collect();

        let kind = match words[0] {
            "nop" => Nop,
            "acc" => Acc,
            "jmp" => Jmp,
            _ => unimplemented!(),
        };
        let arg = if let Some(s) = words[1].strip_prefix("+") {
            s
        } else {
            words[1]
        }
        .parse()
        .unwrap();

        prog.push(Instr { kind, arg });
    }

    prog
}

fn run(prog: &Program) -> (i64, bool) {
    let n = prog.len();

    let mut run: Vec<bool> = vec![false; n];
    let mut pc = 0;
    let mut acc = 0;

    while pc < n && !run[pc] {
        run[pc] = true;

        let instr = &prog[pc];
        match instr.kind {
            Nop => {}
            Acc => acc += instr.arg,
            Jmp => pc = (pc as i64 + instr.arg - 1) as usize,
        }

        pc += 1;
    }

    (acc, pc < n)
}

fn part1(prog: &Program) -> i64 {
    run(prog).0
}

fn part2(prog: &Program) -> i64 {
    let mut prog = prog.clone();

    for i in 0..prog.len() {
        let prev = prog[i].kind;
        let new = match prev {
            Nop => Jmp,
            Jmp => Nop,
            Acc => Acc,
        };

        if new != prev {
            prog[i].kind = new;
            let (acc, infinite) = run(&prog);
            if !infinite {
                return acc;
            }
            prog[i].kind = prev;
        }
    }

    -1
}

fn main() {
    let prog = read_program();

    println!("{}", part1(&prog));
    println!("{}", part2(&prog));
}
