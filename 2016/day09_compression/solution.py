def find_marker(string):
    start = string.find('(')
    if start == -1: return False
    end = string.find(')', start)
    size, repeat = map(int, string[start+1:end].split('x'))
    return start, end, size, repeat

def part1(string):
    marker = find_marker(string)
    if not marker:
        return string
    else:
        start, end, size, repeat = marker
        return string[:start] + \
               string[end+1:end+1+size]*repeat + \
               part1(string[end+1+size:])

def part2(string):
    marker = find_marker(string)
    if not marker:
        return len(string)
    else:
        start, end, size, repeat = marker
        repeat_string = string[end+1:end+1+size]
        return len(string[:start]) + \
               part2(repeat_string)*repeat + \
               part2(string[end+1+size:])

if __name__ == '__main__':
    compressed_string = input()
    print(len(part1(compressed_string)))
    print(part2(compressed_string))
