"""Day 3: Squares With Three Sides

Now that you can think clearly, you move deeper into the labyrinth of hallways
and office furniture that makes up this part of Easter Bunny HQ. This must be a
graphic design department; the walls are covered in specifications for
triangles.

Or are they?

The design document gives the side lengths of each triangle it describes,
but... 5 10 25? Some of these aren't triangles. You can't help but mark the
impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining
side. For example, the "triangle" given above is impossible, because 5 + 10 is
not larger than 25.

In your puzzle input, how many of the listed triangles are possible?

--- Part Two ---

Now that you've helpfully marked up their design documents, it occurs to you
that triangles are specified in groups of three vertically. Each set of three
numbers in a column specifies a triangle. Rows are unrelated.

For example, given the following specification, numbers with the same hundreds
digit would be part of the same triangle:

    101 301 501
    102 302 502
    103 303 503
    201 401 601
    202 402 602
    203 403 60

In your puzzle input, and instead reading by columns, how many of the listed
triangles are possible?
"""

from common import parse_input, quicksort


def convert_input(puzzle_input):
    return [[float(side) for side in triangle.split()] 
            for triangle in puzzle_input.split('\n')][:-1]


def is_triangle(sides):
    a, b, c = quicksort(sides)
    return a + b > c

def count_triangles(triangles):
    return len(list(filter(is_triangle, triangles)))


def part_one(triangles):
    print('Part one -- Triangles:', count_triangles(triangles))


def part_two(triangles):
    triangles_new = []
    triangles = [list(zip(*triangles[i:i+3])) for i in range(0, len(triangles), 3)]
    for triple in triangles:
        for triangle in triple:
            triangles_new.append(triangle)
    print('Part two -- Triangles:', count_triangles(triangles_new))


if __name__ == '__main__':
    triangles = convert_input(parse_input(3))
    part_one(triangles)
    part_two(triangles)
