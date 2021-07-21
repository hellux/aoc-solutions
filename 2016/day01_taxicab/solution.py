from math import pi, e

def convert_input(puzzle_input):
    directions_str = puzzle_input.replace(' ', '').split(',')
    directions = []
    for instruction in directions_str:
        direction = instruction[0]
        rotation = pi/2 if direction == 'L' else -pi/2
        steps = int(instruction[1:])
        directions.append((rotation, steps))
    return directions

def travel(path, direction=0):
    if not path:
        return 0
    else:
        rotation, steps = path[0]
        new_direction = direction + rotation
        return steps*e**(new_direction*1j) + travel(path[1:], new_direction)

def travel_duplicate(path, direction=pi/2):
    pos_hist = []
    position = 0
    for rotation, steps in path:
        direction = direction + rotation
        for _ in range(steps):
            pos_hist.append(position)
            delta = e**(direction*1j)
            position += round(delta.real) + round(delta.imag)*1j
            if position in pos_hist: return position

def blocks_away(position):
    return round(abs(position.real)+abs(position.imag))

def part1(path): return blocks_away(travel(path))
def part2(path): return blocks_away(travel_duplicate(path))

if __name__ == '__main__':
    path = convert_input(input())
    print(part1(path))
    print(part2(path))
