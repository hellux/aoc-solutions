package main

import (
    "io/ioutil"
    "os"
    "fmt"
    "strconv"
)

const (
    WIDTH = 300
    HEIGHT = 300
)

func power_level(x, y, serial int) int {
    rack_id := x + 10
    return (((rack_id*y+serial)*rack_id) % 1000)/100 - 5
}

func part1(serial int) (int, int) {
    max_sum := 0
    max_x, max_y := 0, 0

    for y := 1; y <= HEIGHT-3; y++ {
        for x := 1; x <= WIDTH-3; x++ {
            sum := 0
            for ys := y; ys < y+3; ys++ {
                for xs := x; xs < x+3; xs++ {
                    sum += power_level(xs, ys, serial)
                }
            }

            if sum > max_sum {
                max_sum = sum
                max_x = x
                max_y = y
            }
        }
    }

    return max_x, max_y
}

func part2(serial int) (int, int, int) {
    /* cache power levels */
    powers := make([][]int, HEIGHT)
    for y := 1; y <= HEIGHT; y++ {
        powers[y-1] = make([]int, WIDTH)
        for x := 1; x <= WIDTH; x++ {
            powers[y-1][x-1] = power_level(x, y, serial)
        }
    }

    max_sum := 0
    max_x, max_y, max_size := 0, 0, 0

    for size := 1; size <= WIDTH; size++ {
        for y := 1; y <= HEIGHT-size; y++ {
            /* calc sum for left-most square */
            sum := 0
            for ys := y; ys < y+size; ys++ {
                for xs := 1; xs <= size; xs++ {
                    sum += powers[ys-1][xs-1]
                }
            }

            for x := 1; x <= WIDTH-size; x++ {
                if sum > max_sum {
                    max_sum = sum
                    max_x = x
                    max_y = y
                    max_size = size
                }

                /* remove first column and add next to sum */
                for yc := y; yc < y+size; yc++ {
                    sum -= powers[yc-1][x-1]
                    sum += powers[yc-1][x+size-1]
                }
            }
        }
    }

    return max_x, max_y, max_size
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    serial, _ := strconv.Atoi(string(bytes))

    x, y := part1(serial)
    fmt.Printf("%d,%d\n", x, y)

    x, y, size := part2(serial)
    fmt.Printf("%d,%d,%d\n", x, y, size)
}
