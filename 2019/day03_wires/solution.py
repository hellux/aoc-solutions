dirs = {'R': (1, 0), 'U': (0, 1), 'L': (-1, 0), 'D': (0, -1)}

wire1 = input().split(',')
wire2 = input().split(',')

def follow_wire(wire):
    path = {}
    pos = (0, 0)
    steps = 0
    for instr in wire:
        (dx, dy) = dirs[instr[0]]
        length = int(instr[1:])
        for _ in range(length):
            pos = (pos[0]+dx, pos[1]+dy)
            steps += 1
            if not pos in path:
                path[pos] = steps
    return path            

path1 = follow_wire(wire1)
path2 = follow_wire(wire2)

intersections = {pos:(path1[pos],path2[pos]) for pos in path1 if pos in path2}

manhattan = lambda pos : abs(pos[0])+abs(pos[1])
closest = min(map(manhattan, intersections.keys()))
fastest = min(map(sum, intersections.values()))
print("part1:", closest)
print("part2:", fastest)
