import sys
from collections import namedtuple
import re

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


def part1(addresses):
    support_count = 0
    for address in addresses:
        if supports_tls(address):
            support_count += 1

    return support_count


def part2(addresses):
    support_count = 0
    for address in addresses:
        seqs, hyp_seqs = parse_address_seqs(address)
        if supports_ssl(seqs, hyp_seqs):
            support_count += 1

    return support_count


if __name__ == '__main__':
    addresses = sys.stdin.read().split('\n')
    print(part1(addresses))
    print(part2(addresses))
