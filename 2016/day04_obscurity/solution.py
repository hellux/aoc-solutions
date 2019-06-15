import sys
from collections import namedtuple
from operator import itemgetter


def convert_input(puzzle_input):
    Room = namedtuple('Room', ['id', 'checksum', 'encrypted_name', 'name'])
    rooms = []
    for room in puzzle_input.split('\n'):
        encrypted_name = room[:-11]
        ID = int(room[-10:-7])
        checksum = room[-6:-1]
        name = caesar_shift(encrypted_name, ID)

        rooms.append(Room(ID, checksum, encrypted_name, name))
    return rooms


def get_valid_rooms(rooms):
    return list(
        filter(lambda r: r.checksum == calculate_checksum(r.encrypted_name),
               rooms)
    )


def count_freq(string):
    freqs = {}
    for char in string:
        if char in freqs: freqs[char] += 1
        else: freqs[char] = 1
    return freqs


def calculate_checksum(string):
    freqs = list(count_freq(string.replace('-','')).items())
    freqs_alph = sorted(freqs, key=itemgetter(0))
    freqs_freq_alph = sorted(freqs_alph, key=itemgetter(1), reverse=True)

    return ''.join([x[0] for x in freqs_freq_alph[:5]])


def sum_ids(rooms):
    return sum([room.id for room in rooms])


def caesar_shift(string, n):
    a = ord('a')
    return ''.join([chr(a+((ord(c)-a+n)%26)) if c.isalpha() else c
                    for c in string])


def part_one(rooms):
    print('Part one -- ID sum:', sum_ids(rooms))


def part_two(rooms):
    print('Part two -- North Pole Room:\n')
    for room in rooms:
        if room.name.find('northpole') != -1:
            print(room.name, room.id)


if __name__ == '__main__':
    valid_rooms = get_valid_rooms(convert_input(sys.stdin.read()))
    part_one(valid_rooms)
    part_two(valid_rooms)

