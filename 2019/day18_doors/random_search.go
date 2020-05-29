package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "sort"
    "math"
    "math/rand"
    "time"
)

const (
    WALL = '#'
    EMPTY = '.'
    ENTRANCE = '@'
)

const (
    VEMPTY = iota
    VWALL = iota
    VINTER = iota
    VKEY = iota
    VDOOR = iota
    VENTR = iota
)

type vertex struct {
    id int
    kind int
}

var NODES = []byte{
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '@',
}
var NODE_COUNT = len(NODES)

const (
    KEY_START = 'a'
    KEY_END = 'z'
    DOOR_START = 'A'
    DOOR_END = 'Z'
    KEY_DOOR_DIFF = 'a'-'A'
    ALPH_SIZE = 'z'-'a'+1
)

type node struct {
    tile byte
    x int
    y int
}

type edge struct {
    obstacles []byte
    distance int
}

type graph struct {
    exists []bool
    edges []edge
}

func edge_i(start byte, end byte) int {
    start_i := start-KEY_START
    if start == ENTRANCE {
        start_i = ALPH_SIZE
    }

    end_i := end-KEY_START
    if end == ENTRANCE {
        end_i = ALPH_SIZE
    }

    return int(start_i) * NODE_COUNT + int(end_i)
}

func edge_exists(start byte, end byte, g graph) bool {
    return g.exists[edge_i(start, end)]
}

func get_edge(start byte, end byte, g graph) edge {
    return g.edges[edge_i(start, end)]
}

func set_edge(start byte, end byte, g graph, e edge) graph {
    i := edge_i(start, end)
    g.exists[i] = true
    g.edges[i] = e
    return g
}

func is_prerequisite(prereq byte, node byte, g graph) bool {
    edge := get_edge(ENTRANCE, node, g)
    for _, obst := range edge.obstacles {
        if obst + KEY_DOOR_DIFF == prereq {
            return true
        }
    }
    return false
}

func path_cost(path []byte, g graph) int {
    cost := 0
    current := byte(ENTRANCE)
    for _, node := range path {
        edge := get_edge(current, node, g)
        current = node
        cost += edge.distance
    }
    return cost
}

func permute_valid(nodes []byte, g graph) {
    sorted := false
    for !sorted {
        sorted = true
        n := len(nodes)
        for i := 0; i < n; i++ {
            for j := i+1; j < n; j++ {
                if is_prerequisite(nodes[j], nodes[i], g) {
                    sorted = false
                    nodes[i], nodes[j] = nodes[j], nodes[i]
                }
            }
        }
    }
}

/* assume no cycles */
func naive_a_star(x int, y int,
                  end_x int, end_y int,
                  visited *map[int]bool,
                  vault []string) ([]byte, int) {
    w := len(vault[0])
    obstacles := make([]byte, 0)
    (*visited)[y*w+x] = true

    if x == end_x && y == end_y {
        return obstacles, 0
    } else {
        opts := [][]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
        sort.Slice(opts, func(i, j int) bool {
            dist_i := math.Pow(float64(end_x-opts[i][0]), 2) +
                      math.Pow(float64(end_y-opts[i][1]), 2)
            dist_j := math.Pow(float64(end_x-opts[j][0]), 2) +
                      math.Pow(float64(end_y-opts[j][1]), 2)
            return dist_i < dist_j
        })

        for _, diff := range opts {
            nx, ny := x+diff[0], y+diff[1]
            neigh_tile := vault[ny][nx]
            if neigh_tile == WALL || (*visited)[ny*w+nx] {
                continue
            }
            obst_new, cost := naive_a_star(nx, ny, end_x, end_y,
                                           visited, vault)
            if cost >= 0 {
                if neigh_tile != EMPTY && !(nx == end_x && ny == end_y) {
                    obstacles = append(obstacles, neigh_tile)
                }
                return append(obstacles, obst_new...), cost+1
            }
        }
    }

    return obstacles, -1
}

func create_graph(nodes []node, vault []string) graph {
    ng := NODE_COUNT*NODE_COUNT
    g := graph {
        exists: make([]bool, ng),
        edges: make([]edge, ng),
    }

    n := len(nodes)
    for i := 0; i < n; i++ {
        start := nodes[i]
        for j := i+1; j < n; j++ {
            end := nodes[j]
            visited := make(map[int]bool)
            obstacles, distance := naive_a_star(start.x, start.y,
                                                end.x, end.y,
                                                &visited, vault)
            obstacles_rev := make([]byte, len(obstacles))
            for i, o := range obstacles {
                obstacles_rev[len(obstacles)-i-1] = o
            }

            set_edge(start.tile, end.tile, g, edge{
                obstacles: obstacles,
                distance: distance,
            })
            set_edge(end.tile, start.tile, g, edge{
                obstacles: obstacles_rev,
                distance: distance,
            })
        }
    }

    return g
}

func get_vault() ([]string, []node) {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")

    w, h := len(lines[0]), len(lines)

    nodes := make([]node, 0, NODE_COUNT)
    nodes = append(nodes, node{tile: ENTRANCE, x: 0, y: 0})

    var pos_x, pos_y int
    for y := 0; y < h; y++ {
        for x := 0; x < w; x++ {
            tile := lines[y][x]
            node := node{tile: tile, x: x, y: y}

            if KEY_START <= tile && tile <= KEY_END {
                nodes = append(nodes, node)
            }

            if tile == ENTRANCE {
                pos_x = x
                pos_y = y
            }
        }
    }

    nodes[0].x = pos_x
    nodes[0].y = pos_y

    return lines, nodes
}

func print_chars(chars []byte) {
    for _, c := range chars {
        fmt.Printf("%c", c)
    }
    fmt.Printf("\n")
}

func main() {
    vault, nodes := get_vault()
    g := create_graph(nodes, vault)

    keys := make([]byte, 0)
    for _, n := range nodes {
        if KEY_START <= n.tile && n.tile <= KEY_END {
            keys = append(keys, n.tile)
        }
    }

    min_cost := math.MaxInt32
    rand.Seed(time.Now().UnixNano())
    for {
        rand.Shuffle(len(keys), func(i, j int) {
            keys[i], keys[j] = keys[j], keys[i]
        })
        permute_valid(keys, g)
        cost := path_cost(keys, g)
        if cost < min_cost {
            min_cost = cost
            fmt.Println(cost)
            print_chars(keys)
        }
    }
}
