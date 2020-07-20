use std::io;
use std::collections::HashMap;

type SignalStrength = u16;
type WireName = String;
type Circuit = HashMap<WireName, Gate>;

#[derive(Debug, Clone)]
enum Signal {
    Constant(SignalStrength),
    Wire(WireName)
}

#[derive(Debug, Clone)]
enum Gate {
    Direct(Signal),
    And(Signal, Signal),
    Or(Signal, Signal),
    Not(Signal),
    LShift(Signal, Signal),
    RShift(Signal, Signal),
    Invalid(),
}

fn eval_wire(wire: &str, c: &mut Circuit) -> SignalStrength {
    let gate = &c[wire].clone();
    let signal = match gate {
        Gate::Direct(s) => eval_signal(s, c),
        Gate::And(s1, s2) => eval_signal(s1, c) & eval_signal(s2, c),
        Gate::Or(s1, s2) => eval_signal(s1, c) | eval_signal(s2, c),
        Gate::Not(s) => !eval_signal(s, c),
        Gate::LShift(s1, s2) => eval_signal(s1, c) << eval_signal(s2, c),
        Gate::RShift(s1, s2) => eval_signal(s1, c) >> eval_signal(s2, c),
        Gate::Invalid() => 0,
    };
    
    /* Memoize value of wire. */
    c.insert(wire.to_string(), Gate::Direct(Signal::Constant(signal)));

    signal
}

fn eval_signal(s: &Signal, circuit: &mut Circuit) -> SignalStrength {
    match s {
        Signal::Constant(c) => { *c },
        Signal::Wire(w) => { eval_wire(w, circuit) },
    }
}

fn parse_signal(expr: &str) -> Signal {
    match expr.parse::<SignalStrength>() {
        Ok(num) => { Signal::Constant(num) },
        _ => { Signal::Wire(expr.to_string()) },
    }
}

fn get_circuit() -> io::Result<Circuit> {
    let mut circuit = HashMap::new();

    loop {
        let mut input = String::new();
        let len = io::stdin().read_line(&mut input)?;
        if len == 0 { break };

        let parts: Vec<&str> = input.split(" -> ").collect();
        let expr: Vec<&str> = parts[0].split(" ").collect();
        let key: String = parts[1].chars().filter(|c| *c != '\n').collect();

        let gate = match expr.len() {
            1 => { Gate::Direct(parse_signal(expr[0])) },
            2 => { Gate::Not(parse_signal(expr[1])) },
            3 => {
                let op1 = parse_signal(expr[0]);
                let op2 = parse_signal(expr[2]);
                match expr[1] {
                    "AND" => { Gate::And(op1, op2) },
                    "OR" => { Gate::Or(op1, op2) },
                    "LSHIFT" => { Gate::LShift(op1, op2) },
                    "RSHIFT" => { Gate::RShift(op1, op2) },
                    _ => { Gate::Invalid() },
                }
            },
            _ => { Gate::Invalid() },
        };

        circuit.insert(key, gate);
    }

    Ok(circuit)
}

fn part1(circuit: &mut Circuit) -> SignalStrength {
    eval_wire("a", circuit)
}

fn main() -> io::Result<()> {
    let mut circuit = get_circuit()?;

    println!("{:?}", part1(&mut circuit));

    Ok(())
}
