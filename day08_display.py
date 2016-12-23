"""Day 8: Two-Factor Authentication ---

You come across a door implementing what you can only assume is an
implementation of two-factor authentication after a long game of requirements
telephone.

To get past the door, you first swipe a keycard (no problem; there was one on a
nearby desk). Then, it displays a code on a little screen, and you type that
code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've taken
everything apart and figured out how it works. Now you just have to work out
what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for
the screen; these instructions are your puzzle input. The screen is 50 pixels
wide and 6 pixels tall, all of which start off, and is capable of three somewhat
peculiar operations:

-rect AxB turns on all of the pixels in a rectangle at the top-left of the 
 screen which is A wide and B tall.
-rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right
 by B pixels. Pixels that would fall off the right end appear at the left end of
 the row.
-rotate column x=A by B shifts all of the pixels in column A (0 is the left
 column) down by B pixels. Pixels that would fall off the bottom appear at the
 top of the column.
-For example, here is a simple sequence on a smaller screen:

-rect 3x2 creates a small rectangle in the top-left corner:

###....
###....
.......

-rotate column x=1 by 1 rotates the second column down by one pixel:

#.#....
###....
.#.....

-rotate row y=0 by 4 rotates the top row right by four pixels:

....#.#
###....
.#.....

-rotate column x=1 by 1 again rotates the second column down by one pixel,
 causing the bottom pixel to wrap back to the top:

.#..#.#
#.#....
.#.....

As you can see, this display technology is extremely powerful, and will soon
dominate the tiny-code-displaying-screen market. That's what the advertisement
on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display:
after you swipe your card, if the screen did work, how many pixels should be
lit?

--- Part Two ---

You notice that the screen is only capable of displaying capital letters; in the
font it uses, each letter is 5 pixels wide and 6 tall.

After you swipe your card, what code is the screen trying to display?
"""

from common import parse_input
from time import sleep


PIXEL_ON = '█'
PIXEL_OFF = '░'


def convert_input(puzzle_input):
    return [[token for token in instructions.split()]
                   for instructions in puzzle_input.split('\n')]

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


def draw_screen(instructions, screen):
    if not instructions:
        return screen
    else:
        return draw_screen(instructions[1:],
               eval_instruction(instructions[0], screen))


def display_screen(screen):
    print()
    for row in screen:
        print(row)
    print()

def lit_pixels(screen):
    return sum(row.count(PIXEL_ON) for row in screen)


def part_one(screen):
    print('Part one -- Lit pixels:', lit_pixels(screen))


def part_two(screen):
    print('Part two -- Display:')
    display_screen(screen)

if __name__ == '__main__':
    instructions = convert_input(parse_input(8))
    screen = [PIXEL_OFF*50 for _ in range(6)]
    screen = draw_screen(instructions, screen)
    part_one(screen)
    part_two(screen)
