import sys


def run_program(program, variables):
    return eval_program(parse_program(program), variables)


def parse_program(program):
    return [statement.split() for statement in program.split('\n')]


def eval_program(program, variables):
    pointer = 0
    
    while pointer != len(program):
        statement = program[pointer]
        op = statement[0]
        if op == 'cpy':
            x, y = statement[1:]
            if x.isalpha():
                x = variables[x]
            variables[y] = int(x)
        else:
            reg = statement[1]
            if op == 'inc':
                variables[reg] += 1
            elif op == 'dec':
                variables[reg] -= 1
            elif op == 'jnz':
                jump = int(statement[2])
                x = variables[reg] if reg.isalpha() else int(reg)
                if x:
                    pointer += jump
                    continue
        pointer += 1

    return variables


def part_one(program):
    variables = {var : 0 for var in 'abcd'}
    variables = run_program(program, variables)
    print('Part one -- a:', variables['a'])

 
def part_two(program):
    variables = {var : 0 for var in 'abcd'}
    variables['c'] = 1
    variables = run_program(program, variables)
    print('Part two -- a:', variables['a'])


if __name__ == '__main__':
    program = sys.stdin.read()
    part_one(program)
    part_two(program)
