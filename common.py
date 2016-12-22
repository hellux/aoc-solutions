def read_file(path):
    with open(path) as f:
        return f.read()


def parse_input(day):
    return read_file('input/{:02d}.txt'.format(day))


def quicksort(seq):
    if len(seq) <= 1: return seq

    pivot = seq[0]
    lesser = []
    equal = []
    greater = []

    for scalar in seq:
        if scalar < pivot: lesser.append(scalar)
        elif scalar > pivot: greater.append(scalar)
        else: equal.append(scalar)

    return quicksort(lesser) + equal + quicksort(greater)
