"""Day 7: Internet Protocol Version 7

While snooping around the local network of EBHQ, you compile a list of IP
addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to
figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.
An ABBA is any four-character sequence which consists of a pair of two different
characters followed by the reverse of that pair, such as xyyx or abba. However,
the IP also must not have an ABBA within any hypernet sequences, which are
contained by square brackets.

For example:

-abba[mnop]qrst supports TLS (abba outside square brackets).
-abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even
 though xyyx is outside square brackets).
-aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters
 must be different).
-ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though
 it's within a larger string).

How many IPs in your puzzle input support TLS?
--- Part Two ---

You would also like to know which IPs support SSL (super-secret listening).

An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the
supernet sequences (outside any square bracketed sections), and a corresponding
Byte Allocation Block, or BAB, anywhere in the hypernet sequences. An ABA is any
three-character sequence which consists of the same character twice with a
different character between them, such as xyx or aba. A corresponding BAB is the
same characters but in reversed positions: yxy and bab, respectively.

For example:

-aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab
 within square brackets).
-xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
-aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet;
 the aaa sequence is not related, because the interior character must be
 different).
-zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a
 corresponding bzb, even though zaz and zbz overlap).

How many IPs in your puzzle input support SSL?
"""

from common import parse_input
from collections import namedtuple
import re


def convert_input(puzzle_input):
    return puzzle_input.split('\n')


def parse_address_seqs(address):
    seqs, hyp_seqs = [], []

    brackets_open = [m.start() for m in re.finditer('\[', address)]
    brackets_close = [m.start() for m in re.finditer(']', address)]
    seqs_start = [-1] + brackets_close
    seqs_end = brackets_open + [len(address)]

    for start, end in zip(brackets_open, brackets_close):
        hyp_seqs.append(address[start+1:end])
    for start, end in zip(seqs_start, seqs_end):
        seqs.append(address[start+1:end])

    return seqs, hyp_seqs


def contains_pattern(pattern_test, seq):
    if not seq:
        return False
    else:
        return pattern_test(seq) or contains_pattern(pattern_test, seq[1:])


def any_contains(pattern_test, seqs):
    return any([contains_pattern(pattern_test, seq) for seq in seqs])


def is_abba(seq):
    return len(seq) >= 4 and \
           seq[0] != seq[1] and \
           seq[:2] == seq[3:1:-1]


def is_aba(seq):
    return len(seq) >= 3 and \
           seq[0] != seq[1] and \
           seq[:3] == seq[2::-1]


def corresponding_bab(aba):
    return aba[1] + aba[0] + aba[1]


def has_corresponding_bab(aba, hyp_seqs):
    return any_contains(lambda seq: seq[:3] == corresponding_bab(aba), hyp_seqs)


def supports_tls(address):
    seqs, hyp_seqs = parse_address_seqs(address)
    return any_contains(is_abba, seqs) and \
       not any_contains(is_abba, hyp_seqs)


def find_patterns(pattern_test, seq):
    if not seq:
        return []
    elif pattern_test(seq):
        return [seq] + find_patterns(pattern_test, seq[1:])
    else:
        return find_patterns(pattern_test, seq[1:])


def supports_ssl(seqs, hyp_seqs):
    if not seqs:
        return False
    else:
        for aba in find_patterns(is_aba, seqs[0]):
            if has_corresponding_bab(aba, hyp_seqs):
                return True
        else:
            return supports_ssl(seqs[1:], hyp_seqs)



def part_one(addresses):
    support_count = 0
    for address in addresses:
        if supports_tls(address):
            support_count += 1

    print('Part one -- TLS IPs:', support_count)


def part_two(addresses):
    support_count = 0
    for address in addresses:
        seqs, hyp_seqs = parse_address_seqs(address)
        if supports_ssl(seqs, hyp_seqs):
            support_count += 1

    print('Part two -- SSL IPs:', support_count)


if __name__ == '__main__':
    addresses = convert_input(parse_input(7))
    part_one(addresses)
    part_two(addresses)
