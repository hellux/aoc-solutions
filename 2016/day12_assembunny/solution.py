import sys

def parse_program(program):
    return [statement.split() for statement in program.split('\n')[:-1]]

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

    return variables['a']

def part1(program):
    variables = {var : 0 for var in 'abcd'}
    return eval_program(program, variables)

def part2(program):
    variables = {var : 0 for var in 'abcd'}
    variables['c'] = 1
    return eval_program(program, variables)

if __name__ == '__main__':
    program = parse_program(sys.stdin.read())
    print(part1(program))
    print(part2(program))
