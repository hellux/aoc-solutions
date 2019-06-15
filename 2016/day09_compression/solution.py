def find_marker(string):
    start = string.find('(')
    if start == -1: return False
    end = string.find(')', start)
    size, repeat = map(int, string[start+1:end].split('x'))
    return start, end, size, repeat


def decompression_length(string):
    marker = find_marker(string)
    if not marker:
        return len(string)
    else:
        start, end, size, repeat = marker
        repeat_string = string[end+1:end+1+size]
        return len(string[:start]) + \
               decompression_length(repeat_string)*repeat + \
               decompression_length(string[end+1+size:])


def decompress_v1(string):
    marker = find_marker(string)
    if not marker:
        return string
    else:
        start, end, size, repeat = marker
        return string[:start] + \
               string[end+1:end+1+size]*repeat + \
               decompress_v1(string[end+1+size:])


def part_one(compressed):
    print('Part one -- Length of decompressed input:',
          len(decompress_v1(compressed)))


def part_two(compressed):
    print('Part two -- Length of decompressed input:',
          decompression_length(compressed))


if __name__ == '__main__':
    compressed_string = input()
    part_one(compressed_string)
    part_two(compressed_string)
