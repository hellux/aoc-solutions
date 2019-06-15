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
