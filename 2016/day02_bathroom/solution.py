import sys

def convert_input(puzzle_input):
    mov = {'U': (0, 1), 'R': (1, 0), 'D': (0, -1), 'L': (-1, 0)}
    return [[mov[instr] for instr in line]
            for line in puzzle_input.split('\n')]


def travel(instructions, start, keypad):
    code = ''
    for number in instructions:
        pos = start
        for movement in number:
            row, col = pos[0]+movement[0], pos[1]+movement[1]
            if (row, col) in keypad: pos = (row, col)
        code += keypad[pos]
    return code


def get_code(instructions):
    return ''.join([get_number(instr) for instr in instructions])


def part_one(instructions):
    keypad = {(0, 0): '7', (0, 1): '4', (0, 2): '1',
              (1, 0): '8', (1, 1): '5', (1, 2): '2',
              (2, 0): '9', (2, 1): '6', (2, 2): '3'}

    print('Part one -- Keycode:', travel(instructions, (1, 1), keypad))


def part_two(instructions):
    keypad = {
                                  (2, 4): '1',
                     (1, 3): '2', (2, 3): '3', (3, 3): '4',
        (0, 2): '5', (1, 2): '6', (2, 2): '7', (3, 2): '8', (4, 2): '9',
                     (1, 1): 'A', (2, 1): 'B', (3, 1): 'C',
                                  (2, 0): 'D'
    }

    print('Part two -- Keycode:', travel(instructions, (1, 1), keypad))


if __name__ == '__main__':
    instructions = convert_input(sys.stdin.read())
    part_one(instructions)
    part_two(instructions)
