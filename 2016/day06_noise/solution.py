import sys
from operator import itemgetter


def convert_input(puzzle_input):
    return puzzle_input.split('\n')


def count_freq(string):
    freqs = {}
    for char in string:
        if char in freqs: freqs[char] += 1
        else: freqs[char] = 1
    return freqs


def cancel_noise(data, frequent=True):
    noise_cancelled = ''
    for chars in zip(*data):
        most_frequent = sorted(count_freq(chars).items(),
                               key=itemgetter(1),
                               reverse=frequent)[0][0]
        noise_cancelled += most_frequent
    return noise_cancelled


def part_one(data):
    print('Part one -- Message:', cancel_noise(data))


def part_two(data):
    print('Part two -- Message:', cancel_noise(data, False))


if __name__ == '__main__':
    data = convert_input(sys.stdin.read())
    part_one(data)
    part_two(data)
