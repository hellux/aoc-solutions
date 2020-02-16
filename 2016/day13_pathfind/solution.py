from collections import namedtuple
import heapq
import math

def is_open(pos, seed):
    x, y = pos
    if x < 0 or y < 0: return False
    number = x*x + 3*x + 2*x*y + y + y*y + seed
    return bin(number).count('1') % 2 == 0

def distance(a, b):
    return (a[0]-b[0])**2+(a[1]-b[1])**2

Node = namedtuple("Node", "visited distance")

def dijkstra(start, end, seed):
    nodes = {start: Node(True, 0), end: Node(False, math.inf)}
    unvisited = [start, end]

    while unvisited and not nodes[end].visited:
        unvisited.sort(reverse=True, key=lambda n: nodes[n].distance)
        current = unvisited.pop()

        for option in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            neighbour = (current[0]+option[0], current[1]+option[1])
            if is_open(neighbour, seed):
                if not neighbour in nodes:
                    new_node = Node(False, math.inf)
                    nodes[neighbour] = new_node
                    unvisited.append(neighbour)
                if not nodes[neighbour].visited:
                    distance = nodes[current].distance + 1
                    if distance < nodes[neighbour].distance:
                        nodes[neighbour] = Node(False, distance)

        nodes[current] = Node(True, nodes[current].distance)

    return nodes[end].distance

def explore(start, max_distance, seed):
    nodes = {start: Node(True, 0)}
    unvisited = [start]

    visited = 0
    while unvisited:
        unvisited.sort(reverse=True, key=lambda n: nodes[n].distance)
        current = unvisited.pop()

        if nodes[current].distance > max_distance:
            break

        for option in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            neighbour = (current[0]+option[0], current[1]+option[1])
            if is_open(neighbour, seed):
                if not neighbour in nodes:
                    new_node = Node(False, math.inf)
                    nodes[neighbour] = new_node
                    unvisited.append(neighbour)
                if not nodes[neighbour].visited:
                    distance = nodes[current].distance + 1
                    if distance < nodes[neighbour].distance:
                        nodes[neighbour] = Node(False, distance)

        nodes[current] = Node(True, nodes[current].distance)
        visited += 1

    return visited


def part_one(seed):
    return dijkstra((1, 1), (31, 39), seed)


def part_two(seed):
    return explore((1, 1), 50, seed)


if __name__ == '__main__':
    seed = int(input())
    print(part_one(seed))
    print(part_two(seed))
