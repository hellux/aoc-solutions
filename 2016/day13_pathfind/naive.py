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
    w, h = 120, 30
    sleep(0.3)
    r = lambda x: round(x/30)*30
    draw_room(r(x-w//2), r(y-h//2), r(x+w//2), r(y+h//2), seed, {
        '▓': ((x, y),),
        '▒': previous,
        '░': options,
        'G': (goal,)}
    )


def distance(a, b):
    return (a[0]-b[0])**2+(a[1]-b[1])**2


def find_shortest_path(goal, seed, start=(1, 1), previous=tuple()):
    sx, sy = start
    options = [(sx+x, sy+y) \
               for x, y in ((0, 1), (0, -1), (1, 0), (-1, 0)) \
               if is_open(sx+x, sy+y, seed) and \
                  (sx+x, sy+y) not in previous]

    render(*start, previous, options, goal, seed)

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
    part_one(seed)
