"""Day 1: No Time for a Taxicab

Santa's sleigh uses a very high-precision clock to guide its movements,
and the clock's oscillator is regulated by stars. Unfortunately, the stars
have been stolen... by the Easter Bunny. To save Christmas, Santa needs you
to retrieve all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the advent calendar; the second puzzle is unlocked when you complete
the first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
unfortunately, is as close as you can get - the instructions on the Easter
Bunny Recruiting Document the Elves intercepted start here, and nobody had time
to work them out further.

The Document indicates that you should start at the given coordinates
(where you just landed) and face North. Then, follow the provided sequence:
either turn left (L) or right (R) 90 degrees, then walk forward the given
number of blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so 
you take a moment and work out the destination. Given that you can only walk
on the street grid of the city, how far is the shortest path to the destination?

For example:

-Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
-R2, R2, R2 leaves you 2 blocks due South of your starting position, which is
 2 blocks away.
-R5, L5, R5, R3 leaves you 12 blocks away.

How many blocks away is Easter Bunny HQ?

--- Part Two ---

Then, you notice the instructions continue on the back of the Recruiting
Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you
visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?
"""

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
