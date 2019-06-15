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
    """Calculate position after traveling a path.
    
    Parameters:
        direction -- the current direction you are facing
        path -- a list of instructions you are following:
            rotation -- how much to turn (radians)
            steps -- how far to go in new direction

    Returns:
        a complex number indicating your position on
        the complex number plane
    """
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
    """Calculate how many blocks away you are.
    
    Parameters:
        position -- a complex number indicating where you are
    """
    return round(abs(position.real)+abs(position.imag))


def part_one(path):
    print('Part one --', 'Blocks away:', blocks_away(travel(path)))


def part_two(path):
    print('Part two --', 'Blocks away:', blocks_away(travel_duplicate(path)))


if __name__ == '__main__':
    path = convert_input(input())
    part_one(path)
    part_two(path)
