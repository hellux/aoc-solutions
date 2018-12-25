"""Day 6: Signals and Noise

Something is jamming your communications with Santa. Fortunately, your signal is
only partially jammed, and protocol in situations like this is to switch to a
simple repetition code to get the message through.

In this model, the same message is sent repeatedly. You've recorded the
repeating message signal (your puzzle input), but the data seems quite
corrupted - almost too badly to recover. Almost.

All you need to do is figure out which character is most frequent for each
position. For example, suppose you had recorded the following messages:

eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar

The most common character in the first column is e; in the second, a; in the
third, s, and so on. Combining these characters returns the error-corrected
message, easter.

Given the recording in your puzzle input, what is the error-corrected version of
the message being sent?

--- Part Two ---

Of course, that would be the message - if you hadn't agreed to use a modified
repetition code instead.

In this modified code, the sender instead transmits what looks like random data,
but for each character, the character they actually want to send is slightly
less likely than the others. Even after signal-jamming noise, you can look at
the letter distributions in each column and choose the least common letter to
reconstruct the original message.

In the above example, the least common character in the first column is a; in
the second, d, and so on. Repeating this process for the remaining characters
produces the original message, advent.

Given the recording in your puzzle input and this new decoding methodology, what
is the original message that Santa is trying to send?
"""

from common import parse_input, count_freq
from operator import itemgetter


def convert_input(puzzle_input):
    return puzzle_input.split('\n')


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
    data = convert_input(parse_input(6))
    part_one(data)
    part_two(data)