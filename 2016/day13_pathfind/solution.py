"""--- Day 13: A Maze of Twisty Little Cubicles ---

You arrive at the first floor of this new building to discover a much less
welcoming environment than the shiny atrium of the last one. Instead, you are
in a maze of twisty little cubicles, all alike.

Every location in this area is addressed by a pair of non-negative integers (x
y). Each such coordinate is either a wall or an open space. You can't move
diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward
positive x and y; negative values are invalid, as they represent a location
outside the building. You are in a small waiting area at 1,1.

While it seems chaotic, a nearby morale-boosting poster explains, the layout
is actually quite logical. You can determine whether a given x,y coordinate
will be a wall or an open space using a simple system:

-Find x*x + 3*x + 2*x*y + y + y*y.
-Add the office designer's favorite number (your puzzle input).
-Find the binary representation of that sum; count the number of bits that are
 1.
-If the number of bits that are 1 is even, it's an open space.
-If the number of bits that are 1 is odd, it's a wall.
-For example, if the office designer's favorite number were 10, drawing walls
 as # and open spaces as ., the corner of the building containing 0,0 would
 look like this:

  0123456789
0 .#.####.##
1 ..#..#...#
2 #....##...
3 ###.#.###.
4 .##..#..#.
5 ..##....#.
6 #...##.###

Now, suppose you wanted to reach 7,4. The shortest route you could take is
marked as O:

  0123456789
0 .#.####.##
1 .O#..#...#
2 #OOO.##...
3 ###O#.###.
4 .##OO#OO#.
5 ..##OOO.#.
6 #...##.###

Thus, reaching 7,4 would take a minimum of 11 steps (starting from your
current location, 1,1).

What is the fewest number of steps required for you to reach 31,39?
"""

from time import sleep

WALL = '█'
FLOOR = ' '

def is_open(x, y, seed):
    number = x*x + 3*x + 2*x*y + y + y*y + seed
    return bin(number).count('1') % 2 == 0


def draw_room(start_x, start_y, end_x, end_y, seed, markers=None):
    room = [[FLOOR if is_open(x, y, seed) else WALL
             for x in range(start_x, end_x)]
            for y in range(start_y, end_y)]

    if markers:
        for marker, positions in markers.items():
            for x, y in positions:
                if start_x < x < end_x and \
                   start_y < y < end_y:
                    room[y-start_y][x-start_x] = marker

    for row in room:
        print(''.join(row))


def render(x, y, previous, options, goal, seed):
    w, h = 160, 55
    #sleep(0.3)
    r = lambda x: round(x/30)*30
    draw_room(r(x-w//2), r(y-h//2), r(x+w//2), r(y+h//2), seed, {
        '▓': ((x, y),),
        '▒': previous,
        '░': options,
        'G': (goal,)}
    )


def distance(a, b):
    return ((a[0]-b[0])**2+(a[1]-b[1])**2)**(1/2)


def find_shortest_path(goal, seed, start=(1, 1), previous=tuple()):

    sx, sy = start
    options = [(sx+x, sy+y) \
               for x, y in ((0, 1), (0, -1), (1, 0), (-1, 0)) \
               if is_open(sx+x, sy+y, seed) and \
                  (sx+x, sy+y) not in previous]

    #render(*start, previous, options, goal, seed)

    if not options:
        return None
    elif goal in options or start == goal:
        return 1
    else:
        for option in sorted(options, key=lambda x: distance(x, goal)):
            path = find_shortest_path(goal, seed, option, previous+(start,))
            if path:
                return 1+path
        else:
            return None


def part_one(seed):
    print('Part one -- Shortest path length:',
          find_shortest_path((31, 39), seed))


if __name__ == '__main__':
    seed = int(input())
    seed = 1364
    part_one(seed)
