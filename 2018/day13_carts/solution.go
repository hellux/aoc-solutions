package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "image"
    "sort"
)

var DIR_W = image.Pt(-1, 0)
var DIR_N = image.Pt(0, -1)
var DIR_S = image.Pt(0, 1)
var DIR_E = image.Pt(1, 0)

const (
    EMPTY = ' '
    TRACK_NS = '|'
    TRACK_WE = '-'
    TRACK_NE_SW = '/'
    TRACK_ES_WN = '\\'
    INTERSECTION = '+'
    CART_W = '<'
    CART_N = '^'
    CART_S = 'v'
    CART_E = '>'
    CRASH = 'X'
)

func turn_left(dir image.Point) image.Point { return image.Pt(dir.Y, -dir.X) }
func forward(dir image.Point) image.Point { return dir }
func turn_right(dir image.Point) image.Point { return image.Pt(-dir.Y, dir.X) }

var TURNS = [...]func(image.Point)image.Point{turn_left, forward, turn_right}

type cart struct {
    pos image.Point
    dir image.Point
    intersections int
    crashed bool
}

func tick(tracks [][]byte, carts[]cart) ([]cart, int) {
    carts_new := make([]cart, len(carts))
    copy(carts_new, carts)
    carts = carts_new

    /* sort for processing order top-down and right-left */
    sort.Slice(carts, func(i int, j int) bool {
        if carts[i].pos.Y == carts[j].pos.Y {
            return carts[i].pos.X < carts[j].pos.X
        } else {
            return carts[i].pos.Y < carts[j].pos.Y
        }
    })

    crashes := 0
    for c := 0; c < len(carts); c++ {
        if !carts[c].crashed {
            /* move cart */
            carts[c].pos.X += carts[c].dir.X
            carts[c].pos.Y += carts[c].dir.Y

            /* check for crash */
            for j := 0; j < len(carts); j++ {
                if c != j && !carts[j].crashed {
                    if carts[c].pos.X == carts[j].pos.X &&
                       carts[c].pos.Y == carts[j].pos.Y {
                        crashes += 2
                        carts[c].crashed = true
                        carts[j].crashed = true
                        break
                    }
                }
            }
        }

        /* update direction */
        if !carts[c].crashed {
            tile := tracks[carts[c].pos.Y][carts[c].pos.X]
            switch tile {
            case TRACK_NE_SW:
                switch carts[c].dir {
                case DIR_S, DIR_N:
                    carts[c].dir = turn_right(carts[c].dir)
                case DIR_W, DIR_E:
                    carts[c].dir = turn_left(carts[c].dir)
                }
            case TRACK_ES_WN:
                switch carts[c].dir {
                case DIR_S, DIR_N:
                    carts[c].dir = turn_left(carts[c].dir)
                case DIR_W, DIR_E:
                    carts[c].dir = turn_right(carts[c].dir)
                }
            case INTERSECTION:
                carts[c].dir = TURNS[carts[c].intersections](carts[c].dir)
                carts[c].intersections = (carts[c].intersections + 1) % 3
            }
        }
    }

    return carts, crashes
}

func display(tracks [][]byte, carts []cart) {
    rows := make([][]byte, len(tracks))
    for y := 0; y < len(tracks); y++ {
        rows[y] = make([]byte, len(tracks[y]))
    }

    for y := 0; y < len(tracks); y++ {
        for x := 0; x < len(tracks[0]); x++ {
            rows[y][x] = tracks[y][x]
        }
    }

    for c := 0; c < len(carts); c++ {
        char := '#'
        if carts[c].crashed {
            char = CRASH
        } else {
            switch carts[c].dir {
            case DIR_W:
                char = 17
            case DIR_N:
                char = 17
            case DIR_S:
                char = 17
            case DIR_E:
                char = 17
            }
        }
        rows[carts[c].pos.Y][carts[c].pos.X] = byte(char)
    }

    for _, row := range rows {
        fmt.Println(string(row))
    }
}

func part1(tracks [][]byte, carts []cart) (int, int) {
    crashes := 0
    for crashes == 0 {
        carts, crashes = tick(tracks, carts)
        // display(tracks, carts)
    }

    for _, cart := range carts {
        if cart.crashed {
            return cart.pos.X, cart.pos.Y
        }
    }

    return -1, -1
}

func part2(tracks [][]byte, carts []cart) (int, int) {
    ticks := 0
    total_crashes := 0
    for total_crashes < len(carts)-1 {
        carts_new, crashes := tick(tracks, carts)
        total_crashes += crashes

        /*
        if crashes > 0 {
            fmt.Println("\nbefore:", crashes)
            display(tracks, carts)
            fmt.Println("\nafter:")
            display(tracks, carts_new)
        }
        */

        carts = carts_new
        ticks++
    }

    fmt.Println(ticks)

    for _, cart := range carts {
        if !cart.crashed {
            return cart.pos.X, cart.pos.Y
        }
    }

    return -1, -1
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")

    /* XXX first line must have spaces appended to max width */
    width := len(lines[0])
    height := len(lines)

    /* create slices of tracks and carts separately */
    carts := make([]cart, 0, 10)
    tracks := make([][]byte, height)
    for y := 0; y < height; y++ {
        tracks[y] = make([]byte, width)
        for x := 0; x < width; x++ {
            switch lines[y][x] {
            case CART_W:
                tracks[y][x] = TRACK_WE
                carts = append(carts, cart{image.Pt(x, y), DIR_W, 0, false})
            case CART_N:
                tracks[y][x] = TRACK_NS
                carts = append(carts, cart{image.Pt(x, y), DIR_N, 0, false})
            case CART_S:
                tracks[y][x] = TRACK_NS
                carts = append(carts, cart{image.Pt(x, y), DIR_S, 0, false})
            case CART_E:
                tracks[y][x] = TRACK_WE
                carts = append(carts, cart{image.Pt(x, y), DIR_E, 0, false})
            default:
                tracks[y][x] = lines[y][x]
            }
        }
    }

    // display(tracks, carts)

    x, y := part1(tracks, carts)
    fmt.Printf("%d,%d\n", x, y)

    x, y = part2(tracks, carts)
    fmt.Printf("%d,%d\n", x, y)
}
