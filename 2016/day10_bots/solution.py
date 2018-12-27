"""Day 10: Balance Bots

You come upon a factory in which many robots are zooming around handing small
microchips to each other.

Upon closer examination, you notice that each bot only proceeds when it has two
microchips, and once it does, it gives each one to a different bot or puts it in
a marked "output" bin. Sometimes, bots take microchips from "input" bins, too.

Inspecting one of the microchips, it seems like they each contain a single
number; the bots must use some logic to decide what to do with each chip. You
access the local control computer and download the bots' instructions (your
puzzle input).

Some of the instructions specify that a specific-valued microchip should be
given to a specific bot; the rest of the instructions indicate what a given bot
should do with its lower-value or higher-value chip.

For example, consider the following instructions:

value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2

-Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2
 chip and a value-5 chip.
-Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its
 higher one (5) to bot 0.
-Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives
 the value-3 chip to bot 0.
-Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in
 output 0.
-In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a
 value-2 microchip, and output bin 2 contains a value-3 microchip. In this
 configuration, bot number 2 is responsible for comparing value-5 microchips
 with value-2 microchips.

Based on your instructions, what is the number of the bot that is responsible
for comparing value-61 microchips with value-17 microchips?
"""

import sys

def convert_input(puzzle_input):
    return puzzle_input.split('\n')


def find_start_count(instructions):
    bots = []

    start = 0
    top = 0
    for instr in instructions:
        tokens = instr.split()
        if len(tokens) == 6 and tokens[0] == 'value':
            bot = int(tokens[5])
            if bot in bots:
                start = bot
            else:
                bots.append(bot)
            top = max(top, bot)

    return start, top+1


def find_dealers(instructions, bot):
    dealers = []
    for instr in instructions:
        tokens = instr.split()
        if tokens[-1] == str(bot) and tokens[-2] == 'bot' or \
           (len(tokens) > 7 and tokens[6] == str(bot) and tokens[5] == 'bot'):
            print(instr, bot)
            if tokens[0] == 'value':
                dealers.append(int(tokens[1]))
            elif tokens[0] == 'bot':
                hl_str = tokens[tokens.index(str(bot))-3]
                high_low = HIGH if hl_str == 'high' else LOW
                dealers.append((tokens[1], high_low))
            else:
                raise ValueError('Invalid instr')
    assert len(dealers) == 2, dealers
    return dealers


def simulate(instructions):

    bots = {}
    outputs = {}

    start, count = find_start_count(instructions)

    def has_values(bot):
        return bot in bots and len(bots[bot]) == 2

    def get_value(bot, high_low=0):
        if not has_values(bot):
            dealers = find_dealers(instructions, bot)
            values = []
            for d in dealers:
                if isinstance(d, tuple):
                    values.append(get_value(*d))
                elif isinstance(d, int):
                    values.append(d)
                else:
                    raise ValueError('Invalid dealer!')
            low = min(values)
            high = max(values)
            bots[bot] = (low, high)

        return bots[bot][high_low]

    for bot in range(count):
        get_value(bot)

    return bots


def part_one(bots):
    for bot, values in bots.items():
        if values == (17, 61):
            print(bot)


if __name__ == '__main__':
    LOW = 0
    HIGH = 1

    instructions = convert_input(sys.stdin.read())
    bots = simulate(instructions)
    part_one(bots)
