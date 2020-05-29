package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
//    "sort"
//    "math"
//    "math/rand"
//    "time"
)

const (
    WALL = '#'
    EMPTY = '.'
    ENTRANCE = '@'
)

const (
    VKEY = iota
    VDOOR = iota
    VEMPTY = iota
    VWALL = iota
    VINTER = iota
    VENTR = iota
)

type vertex struct {
    kind int
    id int
}

const ENTRANCE_ID = ALPH_SIZE
const NODE_COUNT = ALPH_SIZE+1

const (
    KEY_START = 'a'
    KEY_END = 'z'
    DOOR_START = 'A'
    DOOR_END = 'Z'
    KEY_DOOR_DIFF = 'a'-'A'
    ALPH_SIZE = 'z'-'a'+1
)

type node struct {
    id int
    x int
    y int
}

type edge struct {
    obstacles []vertex
    distance int
}

type graph struct {
    exists []bool
    edges []edge
}

func edge_i(start int, end int) int {
    return start*NODE_COUNT + end
}

func edge_exists(start int, end int, g graph) bool {
    return g.exists[edge_i(start, end)]
}

func get_edge(start int, end int, g graph) edge {
    return g.edges[edge_i(start, end)]
}

func set_edge(start int, end int, g graph, e edge) graph {
    i := edge_i(start, end)
    g.exists[i] = true
    g.edges[i] = e
    return g
}

func path_cost(path []byte, g graph) int {
    cost := 0
    current := int(ENTRANCE_ID)
    for _, node := range path {
        i := int(node) - KEY_START
        edge := get_edge(current, i, g)
        current = i
        cost += edge.distance
    }
    return cost
}

type search struct {
    x int
    y int
    cost int
    obst []vertex
}

func bfs(x int, y int, end_x int, end_y int,
         visited *map[int]bool, vault [][]vertex) ([]vertex, int) {
    dirs := [][]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
    w := len(vault[0])

    queue := make([]search, 0)
    initial := search{x, y, 0, make([]vertex, 0)}
    queue = append(queue, initial)

    for len(queue) != 0 {
        s := queue[0]
        queue = queue[1:]

        (*visited)[s.y*w + s.x] = true
        sv := vault[s.y][s.x]

        if s.x == end_x && s.y == end_y {
            return s.obst, s.cost
        } else {
            for _, dir := range dirs {
                nx := s.x + dir[0]
                ny := s.y + dir[1]

                if vault[ny][nx].kind != VWALL && !(*visited)[ny*w + nx] {
                    new_obst := make([]vertex, len(s.obst))
                    copy(new_obst, s.obst)
                    if sv.kind != VEMPTY && !(s.x == x && s.y == y) {
                        new_obst = append(new_obst, sv)
                    }
                    new_search := search{nx, ny, s.cost+1, new_obst}
                    queue = append(queue, new_search)
                }
            }
        }
    }

    return make([]vertex, 0), -1
}

func create_graph(nodes []node, vault [][]vertex) graph {
    ne := NODE_COUNT*NODE_COUNT
    g := graph {
        exists: make([]bool, ne),
        edges: make([]edge, ne),
    }

    n := len(nodes)
    for i := 0; i < n; i++ {
        start := nodes[i]
        for j := i+1; j < n; j++ {
            end := nodes[j]
            visited := make(map[int]bool)
            obstacles, distance := bfs(start.x, start.y,
                                       end.x, end.y,
                                       &visited, vault)
            obstacles_rev := make([]vertex, len(obstacles))
            for i, o := range obstacles {
                obstacles_rev[len(obstacles)-i-1] = o
            }

            set_edge(start.id, end.id, g, edge{
                obstacles: obstacles,
                distance: distance,
            })
            set_edge(end.id, start.id, g, edge{
                obstacles: obstacles_rev,
                distance: distance,
            })
        }
    }

    return g
}

func get_vault() ([][]vertex, []node) {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")

    w, h := len(lines[0]), len(lines)

    vertices := make([][]vertex, h)
    for row := 0; row < h; row++ {
        vertices[row] = make([]vertex, w)
    }

    nodes := make([]node, 0, NODE_COUNT)
    dirs := [][]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}

    intersections := 0
    for y := 0; y < h; y++ {
        for x := 0; x < w; x++ {
            tile := lines[y][x]
            var kind int
            var id int

            if KEY_START <= tile && tile <= KEY_END {
                kind = VKEY
                id = int(tile)-KEY_START
                nodes = append(nodes, node{id, x, y})
            } else if DOOR_START <= tile && tile <= DOOR_END {
                kind = VDOOR
                id = int(tile)-DOOR_START
            } else if tile == ENTRANCE {
                kind = VENTR
                id = ENTRANCE_ID
                nodes = append(nodes, node{id, x, y})
            } else if tile == EMPTY {
                valid_directions := 0
                for _, dir := range dirs {
                    nx := x + dir[0]
                    ny := y + dir[1]
                    if lines[ny][nx] != WALL {
                        valid_directions++
                    }
                }
                if valid_directions > 2 {
                    kind = VINTER
                    id = intersections
                    intersections++
                } else {
                    kind = VEMPTY
                    id = 0
                }
            } else if tile == WALL {
                kind = VWALL
                id = 0
            } else {
            }

            vertices[y][x] = vertex{kind, id}
        }
    }

    return vertices, nodes
}

func vertex_str(v vertex) string {
    switch v.kind {
    case VEMPTY:    return fmt.Sprintf("%c", EMPTY)
    case VKEY:      return fmt.Sprintf("%c", KEY_START + byte(v.id))
    case VDOOR:     return fmt.Sprintf("%c", DOOR_START + byte(v.id))
    case VINTER:    return fmt.Sprintf("%d", v.id)
    case VWALL:     return fmt.Sprintf("%c", WALL)
    case VENTR:     return fmt.Sprintf("%c", ENTRANCE)
    default:        return fmt.Sprintf("%c", EMPTY)
    }
}

func vertex_char(v vertex) byte {
    if v.kind == VINTER {
        return '%'
    } else {
        return vertex_str(v)[0]
    }
}

func print_vault(vault [][]vertex) {
    h := len(vault)
    w := len(vault[0])
    for y := 0; y < h; y++ {
        for x := 0; x < w; x++ {
            v := vault[y][x]
            fmt.Printf("%c", vertex_char(v))
        }
        fmt.Println()
    }
}

func main() {
    vault, nodes := get_vault()
    g := create_graph(nodes, vault)

    print_vault(vault)

    sv := vertex{VENTR, ENTRANCE_ID}
    for _, end := range nodes {
        ev := vault[end.y][end.x]
        if edge_exists(sv.id, ev.id, g) {
            edge := get_edge(sv.id, ev.id, g)
            fmt.Printf("%s-", vertex_str(sv))
            for _, o := range edge.obstacles {
                fmt.Printf("%s-", vertex_str(o))
            }
            fmt.Printf("%s\n", vertex_str(ev))
        }
    }

    p0 := []byte("tgrjdoyaukbhcivpezwqxfsmln")

    fmt.Println(path_cost(p0, g))

    p1 := []byte("orjdvp")
    p2 := []byte("ukbh")
    p3 := []byte("yaciqxn")
    p4 := []byte("tgezwfsml")
    total_cost := path_cost(p1, g)-2 +
                  path_cost(p2, g)-2 +
                  path_cost(p3, g)-2 +
                  path_cost(p4, g)-2
    fmt.Println(total_cost)
}
