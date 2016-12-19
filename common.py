def read_file(path):
    with open(path) as f:
        return f.read()


def parse_input(day):
    return read_file('input/{:02d}.txt'.format(day))
