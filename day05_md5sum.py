"""Day 5: How About a Nice Game of Chess?

You are faced with a security door designed by Easter Bunny engineers that seem
to have acquired most of their security knowledge by watching hacking movies.

The eight-character password for the door is generated one character at a time
by finding the MD5 hash of some Door ID (your puzzle input) and an increasing
integer index (starting with 0).

A hash indicates the next character in the password if its hexadecimal
representation starts with five zeroes. If it does, the sixth character in the
hash is the next character of the password.

For example, if the Door ID is abc:

-The first index which produces a hash that starts with five zeroes is 3231929,
 which we find by hashing abc3231929; the sixth character of the hash, and thus
 the first character of the password, is 1.
-5017308 produces the next interesting hash, which starts with 000008f82..., so
 the second character of the password is 8.
-The third time a hash starts with five zeroes is for abc5278568, discovering
 the character f.

In this example, after continuing this search a total of eight times, the
password is 18f47a30.

Given the actual Door ID, what is the password?

--- Part Two ---

As the door slides open, you are presented with a second door that uses a
slightly more inspired security mechanism. Clearly unimpressed by the last
version (in what movie is the password decrypted in order?!), the Easter Bunny
engineers have worked out a better solution.

Instead of simply filling in the password from left to right, the hash now also
indicates the position within the password to fill. You still look for hashes
that begin with five zeroes; however, now, the sixth character represents the
position (0-7), and the seventh character is the character to put in that
position.

A hash result of 000001f means that f is the second character in the password.
Use only the first result for each position, and ignore invalid positions.

For example, if the Door ID is abc:

-The first interesting hash is from abc3231929, which produces 0000015...; so, 5
 goes in position 1: _5______.
-In the previous method, 5017308 produced an interesting hash; however, it is
 ignored, because it specifies an invalid position (8).
The second interesting hash is at index 5357525, which produces 000004e...; so,
 e goes in position 4: _5__e___.
-You almost choke on your popcorn as the final character falls into place,
 producing the password 05ace8e3.

Given the actual Door ID and this new method, what is the password? Be extra
proud of your solution if it uses a cinematic "decrypting" animation.
"""

from common import parse_input
from hashlib import md5

from random import randint

        
def match_iterate_hex(seq_start, hex_start, index, animate):
    md5_sum = ''
    while md5_sum[:len(hex_start)] != hex_start:
        string = (seq_start + str(index)).encode('utf-8')
        md5_sum = md5(string).hexdigest()
        index += 1
        if index % 10000 == 0: animate()

    return md5_sum, index


def find_password(password_length, hex_start, seq_start, pw_assign):
    def animate(pw):
        string = ''
        for char in pw:
            string += chr(randint(ord('a'), ord('z'))) \
                      if char == '_' else char
        print('\r'+string, end='')

    password, index = '_'*password_length, 0
    while password.find('_') != -1:
        while True:
            md5_sum, index = match_iterate_hex(
                seq_start,
                hex_start,
                index,
                lambda: animate(password)
            )
            new_password = pw_assign(password, md5_sum)
            if password != new_password:
                password = new_password
                break
    return password


def part_one(door_id):
    def pw_assign(pw, md5):
        index = pw.find('_')
        char = md5[5]
        return pw[:index] + char + pw[index+1:]

    print('\rPart one -- Password:',
          find_password(8, '00000', door_id, pw_assign))


def part_two(door_id):
    def pw_assign(pw, md5):
        index = md5[5]
        if index.isnumeric() and \
           int(index) < len(pw) and \
           pw[int(index)] == '_':
            index = int(index)
            char = md5[6]
            return pw[:index] + char + pw[index+1:]
        else:
            return pw

    print('\rPart two -- Password:',
          find_password(8, '00000', door_id, pw_assign))


if __name__ == '__main__':
    door_id = parse_input(5)
    #part_one(door_id)
    part_two(door_id)
