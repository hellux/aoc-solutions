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
"""

from common import parse_input
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
            steps -- how far to go

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


def blocks_away(position):
    """Calculate how many blocks away you are.
    
    Parameters:
        position -- a complex number indicating where you are
    """
    return round(abs(position.real)+abs(position.imag))


def no_time_for_taxicab():
    puzzle_input = parse_input(1)
    path = convert_input(puzzle_input)
    print('Blocks away:', blocks_away(travel(path)))


if __name__ == '__main__':
    no_time_for_taxicab()
