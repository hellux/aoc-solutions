"""Day 4: Security Through Obscurity 
Finally, you come across an information kiosk with a list of rooms. Of course,
the list is encrypted and full of decoy data, but the instructions to decode the
list are barely hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes)
followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in
the encrypted name, in order, with ties broken by alphabetization. For example:

-aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a
 (5), b (3), and then a tie between x, y, and z, which are listed
  alphabetically.
-a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all
 tied (1 of each), the first five are listed alphabetically.
-not-a-real-room-404[oarel] is a real room.
-totally-real-room-200[decoy] is not.

Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?

--- Part Two ---

With all the decoy data out of the way, it's time to decrypt this list and get
moving.

The room names are encrypted by a state-of-the-art shift cipher, which is nearly
unbreakable without the right software. However, the information kiosk designers
at Easter Bunny HQ were not expecting to deal with a master cryptographer like
yourself.

To decrypt a room name, rotate each letter forward through the alphabet a number
of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A,
and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.

What is the sector ID of the room where North Pole objects are stored?
"""

from common import parse_input
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


def calculate_checksum(string):
    freqs = list(count_freq(string.replace('-','')).items())
    freqs_alph = sorted(freqs, key=itemgetter(0))
    freqs_freq_alph = sorted(freqs_alph, key=itemgetter(1), reverse=True)

    return ''.join([x[0] for x in freqs_freq_alph[:5]])


def count_freq(string):
    freqs = {}
    for char in string:
        if char in freqs: freqs[char] += 1
        else: freqs[char] = 1
    return freqs


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
    valid_rooms = get_valid_rooms(convert_input(parse_input(4)))
    part_one(valid_rooms)
    part_two(valid_rooms)

