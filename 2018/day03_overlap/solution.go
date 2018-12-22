package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "sort"
)

type claim struct {
    id int
    x int
    y int
    width int
    height int
    overlaps bool
}

type square struct {
    x int
    y int
    claim *claim
}

func part12(claims []claim, n int) (int, int) {
    /* list claimed squares */
    squares := make([]square, 0, 10000)
    for i := 0; i < n; i++ {
        c := &claims[i]
        for y := c.y; y < c.y+c.height; y++ {
            for x := c.x; x < c.x+c.width; x++ {
                squares = append(squares, square{x, y, c})
            }
        }
    }

    /* sort squares by position */
    sort.Slice(squares, func(i int, j int) bool {
        if squares[i].x == squares[j].x {
            return squares[i].y < squares[j].y
        } else {
            return squares[i].x < squares[j].x
        }
    })

    /* count square overlaps and mark overlapping claims */
    overlaps := 0
    square_prev := squares[0]
    overlap := false
    for i := 1; i < len(squares); i++ {
        if squares[i].x == square_prev.x &&
           squares[i].y == square_prev.y {
            squares[i].claim.overlaps = true
            if !overlap {
                overlaps++
                overlap = true
            }
        } else {
            overlap = false
            square_prev = squares[i]
        }
    }

    intact := 0
    /* find non-overlapping claim */
    for i := 0; i < n; i++ {
        if !claims[i].overlaps {
            intact = claims[i].id
            break
        }
    }

    return overlaps, intact
}

type tree struct {
    left *tree
    x int
    y int
    overlap bool
    right *tree
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")
    n := len(lines)

    claims := make([]claim, n, n)
    for i := 0; i < n; i++ {
        fmt.Sscanf(lines[i], "#%d @ %d,%d: %dx%d",
            &claims[i].id,
            &claims[i].x,
            &claims[i].y,
            &claims[i].width,
            &claims[i].height)
    }

    fmt.Println(part12(claims, n))
}
