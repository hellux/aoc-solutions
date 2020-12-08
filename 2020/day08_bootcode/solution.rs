use self::InstrKind::*;

#[derive(Debug)]
enum InstrKind {
    Nop,
    Acc,
    Jmp,
}
#[derive(Debug)]
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

fn part1(prog: Program) -> i64 {
    let mut run: Vec<bool> = vec![false; prog.len()];
    let mut pc = 0;
    let mut acc = 0;

    while !run[pc] {
        let instr = &prog[pc];
        match instr.kind {
            Nop => {}
            Acc => acc += instr.arg,
            Jmp => pc = (pc as i64 + instr.arg - 1) as usize,
        }
        run[pc] = true;
        pc += 1;
    }

    acc
}

fn main() {
    let prog = read_program();

    println!("{}", part1(prog));
}
