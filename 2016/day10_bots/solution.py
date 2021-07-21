import sys

def bot_count(instructions):
    top = 0
    for instr in instructions:
        tokens = instr.split()
        if tokens[0] == 'bot':
            top = max(int(tokens[1]), top)
        if tokens[5] == 'bot':
            top = max(int(tokens[6]), top)
        if len(tokens) == 12 and tokens[10] == 'bot':
            top = max(int(tokens[11]), top)

    return top+1

def find_dealers(instructions, bot):
    dealers = []
    for instr in instructions:
        tokens = instr.split()
        if tokens[-1] == str(bot) and tokens[-2] == 'bot' or \
           (len(tokens) > 7 and tokens[6] == str(bot) and tokens[5] == 'bot'):
            if tokens[0] == 'value':
                dealers.append(int(tokens[1]))
            elif tokens[0] == 'bot':
                hl_str = tokens[tokens.index(str(bot))-3]
                high_low = 1 if hl_str == 'high' else 0
                dealers.append((tokens[1], high_low))
            else:
                raise ValueError('Invalid instr')
    assert len(dealers) == 2, dealers
    return dealers

def simulate(instructions):
    def get_value(bot, high_low=0):
        if bot not in bots or len(bots[bot]) < 2:
            dealers = find_dealers(instructions, bot)
            values = []
            for d in dealers:
                if isinstance(d, tuple):
                    values.append(get_value(*d))
                elif isinstance(d, int):
                    values.append(d)
                else:
                    raise ValueError('Invalid dealer!')
            bots[bot] = (min(values), max(values))

        return bots[bot][high_low]

    bots = {}
    for bot in range(bot_count(instructions)):
        get_value(bot)

    outputs = []
    for o in [0, 1, 2]:
        for instr in instructions:
            tokens = instr.split()
            if tokens[0] == 'bot':
                bot = bots[int(tokens[1])]
                if tokens[5] == 'output' and tokens[6] == str(o):
                    outputs.append(bot[0])
                elif tokens[10] == 'output' and tokens[11] == str(o):
                    outputs.append(bot[1])

    return bots, outputs

def part1(bots):
    for bot, values in bots.items():
        if values == (17, 61):
            return bot

def part2(outputs):
    return outputs[0] * outputs[1] * outputs[2]

if __name__ == '__main__':
    instructions = sys.stdin.read().split('\n')[:-1]
    bots, outputs = simulate(instructions)

    print(part1(bots))
    print(part2(outputs))
