import sys
from time import sleep

PIXEL_ON = '█'
PIXEL_OFF = '░'

def convert_input(puzzle_input):
    return [[token for token in instructions.split()]
                for instructions in puzzle_input.split('\n')[:-1]]

def screen_transponate(screen):
    return [''.join(row) for row in zip(*screen)]

def rotate(string, n):
    n %= len(string)
    return string[-n:] + string[:-n]

def draw_rect(w, h, screen):
    screen = screen.copy()
    for i in range(h):
        screen[i] = PIXEL_ON*w + screen[i][w:]
    return screen

def eval_rotation(instruction, screen):
    screen = screen.copy()
    _, axis, coord, _, n = instruction
    coord = int(coord[coord.find('=')+1:])
    n = int(n)
    if axis == 'row':
        screen[coord] = rotate(screen[coord], n)
    elif axis == 'column':
        screen = screen_transponate(screen)
        screen[coord] = rotate(screen[coord], n)
        screen = screen_transponate(screen)
    else:
        raise SyntaxError('Invalid axis! -- {}'.format(axis))

    return screen

def eval_drawing(instruction, screen):
    _, dimensions = instruction
    w, h = map(int, dimensions.split('x'))
    return draw_rect(w, h, screen)

def eval_instruction(instruction, screen):
    eval_fn = {'rotate': eval_rotation,
               'rect': eval_drawing}

    return eval_fn[instruction[0]](instruction, screen)

def draw(instructions, screen=[PIXEL_OFF*50 for _ in range(6)]):
    if not instructions:
        return screen
    else:
        return draw(instructions[1:],
               eval_instruction(instructions[0], screen))

def lit_pixels(screen):
    return sum(row.count(PIXEL_ON) for row in screen)

if __name__ == '__main__':
    instructions = convert_input(sys.stdin.read())
    screen = draw(instructions)

    print(lit_pixels(screen))
    for row in screen:
        print(row)
