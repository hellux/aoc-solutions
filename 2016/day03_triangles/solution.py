import sys

def convert_input(puzzle_input):
    return [[float(side) for side in triangle.split()]
            for triangle in puzzle_input.split('\n')][:-1]

def quicksort(seq):
    if len(seq) <= 1: return seq

    pivot = seq[0]
    lesser = []
    equal = []
    greater = []

    for scalar in seq:
        if scalar < pivot: lesser.append(scalar)
        elif scalar > pivot: greater.append(scalar)
        else: equal.append(scalar)

    return quicksort(lesser) + equal + quicksort(greater)

def is_triangle(sides):
    a, b, c = quicksort(sides)
    return a + b > c

def count_triangles(triangles):
    return len(list(filter(is_triangle, triangles)))

def part1(triangles): return count_triangles(triangles)

def part2(triangles):
    triangles_new = []
    triangles = [list(zip(*triangles[i:i+3])) for i in range(0, len(triangles), 3)]
    for triple in triangles:
        for triangle in triple:
            triangles_new.append(triangle)
    return count_triangles(triangles_new)

if __name__ == '__main__':
    triangles = convert_input(sys.stdin.read())
    print(part1(triangles))
    print(part2(triangles))
