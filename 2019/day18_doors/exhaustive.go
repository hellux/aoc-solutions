package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "math"
    "container/heap"
)

const (
    WALL = '#'
    EMPTY = '.'
    ENTRANCE = '@'
)

const (
    KEY_START = 'a'
    KEY_END = 'z'
    DOOR_START = 'A'
    DOOR_END = 'Z'
    KEY_DOOR_DIFF = 'a'-'A'
)

type node struct {
    tile byte
    cost int
    x int
    y int
}

type node_heap []*node
func (s node_heap) Len() int { return len(s) }
func (s node_heap) Swap(i, j int) { s[i], s[j] = s[j], s[i] }
func (s node_heap) Less(i, j int) bool { return s[i].cost < s[j].cost }
func (h *node_heap) Push(x interface{}) { *h = append(*h, x.(*node))}
func (h *node_heap) Pop() interface{} {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0:n-1]
	return x
}

type tiles map[byte]bool

func reachable(target tiles, obstruction tiles,
               x int, y int, vault []string) node_heap {
    width := len(vault[0])
    height := len(vault)

    n := width*height
    nodes := make(map[int]*node)
    unvisited := make(node_heap, 0, n)
    visited := make(map[int]bool)
    found := make(node_heap, 0, n)

    start := &node{
        tile: vault[y][x],
        cost: 0,
        x: x,
        y: y,
    }

    i_start := y*width+x
    nodes[i_start] = start
    heap.Push(&unvisited, start)

    opts := [][]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}

    var current *node

    for len(unvisited) > 0 {
        current = heap.Pop(&unvisited).(*node)

        if target[current.tile] {
            heap.Push(&found, current)
        }

        for _, diff := range opts {
            xn := current.x + diff[0]
            yn := current.y + diff[1]
            neigh_tile := vault[yn][xn]

            if neigh_tile == WALL || obstruction[neigh_tile] {
                continue
            }

            neigh_i := yn*width + xn
            neigh, seen := nodes[neigh_i]
            neigh_cost := current.cost + 1;

            if !seen {
                nodes[neigh_i] = &node{
                    tile: neigh_tile,
                    cost: neigh_cost,
                    x: xn,
                    y: yn,
                }
                neigh = nodes[neigh_i]
                heap.Push(&unvisited, neigh)
            }

            if !visited[neigh_i] && neigh_cost < neigh.cost {
                neigh.cost = neigh_cost
            }
        }

        current_i := current.y*width + current.x
        visited[current_i] = true;
    }

    return found
}

func obtain_keys(x int, y int, remaining tiles, locked tiles,
                 cost int, min_cost *int, vault []string) {
    keys := reachable(remaining, locked, x, y, vault)

    if len(keys) == 0 {
        if cost < *min_cost {
            *min_cost = cost
            fmt.Println(cost)
        }
    }

    for len(keys) > 0 {
        key := heap.Pop(&keys).(*node)

        key_cost := cost + key.cost

        if key_cost >= *min_cost {
            return
        }

        remaining[key.tile] = false
        locked[key.tile - KEY_DOOR_DIFF] = false

        obtain_keys(key.x, key.y, remaining, locked,
                    key_cost, min_cost, vault)

        remaining[key.tile] = true
        locked[key.tile - KEY_DOOR_DIFF] = true
    }
}

func get_vault() ([]string, tiles, tiles, int, int) {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")

    keys := make(tiles)
    doors := make(tiles)
    var pos_x, pos_y int
    for y := 0; y < len(lines); y++ {
        for x := 0; x < len(lines[y]); x++ {
            tile := lines[y][x]

            if KEY_START <= tile && tile <= KEY_END {
                keys[tile] = true
            }

            if DOOR_START <= tile && tile <= DOOR_END {
                doors[tile] = true
            }

            if tile == ENTRANCE {
                pos_x = x
                pos_y = y
            }
        }
    }

    return lines, keys, doors, pos_x, pos_y
}

func print_tiles(t tiles) {
    fmt.Printf("(")
    for c, _ := range t {
        fmt.Printf("%c", c)
    }
    fmt.Println(")");
}

func main() {
    vault, keys, doors, x, y := get_vault()

    min_cost := math.MaxInt32;
    obtain_keys(x, y, keys, doors, 0, &min_cost, vault)

    fmt.Println(min_cost)
}
