package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "math"
)

type point struct {
    x float64
    y float64
}

func part1(points []point, n int) int {
    /* find total range of all points */
    min_x := points[0].x; max_x := points[0].x
    min_y := points[0].y; max_y := points[0].y
    for i := 1; i < n; i++ {
        if points[i].x < min_x {
            min_x = points[i].x
        }
        if points[i].x > max_x {
            max_x = points[i].x
        }
        if points[i].y < min_y {
            min_y = points[i].y
        }
        if points[i].y > max_y {
            max_y = points[i].y
        }
    }
    width := max_x-min_x+1
    height := max_y-min_y+1

    /* count closest squares for each point */
    areas := make([]int, n, n)
    for y := min_y; y <= max_y; y++ {
        for x := min_x; x <= max_x; x++ {
            closest := -1
            close_count := 0
            min_distance := width+height+1
            for i := 0; i < n; i++ {
                distance := math.Abs(points[i].x-x)+math.Abs(points[i].y-y)
                if distance == min_distance {
                    close_count++
                } else if distance < min_distance {
                    min_distance = distance
                    closest = i
                    close_count = 1
                }
            }

            if close_count == 1 {
                if areas[closest] < 0 || x == min_x || x == max_x ||
                                         y == min_y || y == max_y {
                    areas[closest] = -1
                } else {
                    areas[closest]++
                }
            }
        }
    }

    max_area := 0
    for i := 0; i < n; i++ {
        if areas[i] > max_area {
            max_area = areas[i]
        }
    }

    return max_area
}

func part2(points []point, n int) int {
    const MAX_DISTANCE = 10000
    /* find total range of all points */
    min_x := points[0].x; max_x := points[0].x
    min_y := points[0].y; max_y := points[0].y
    for i := 1; i < n; i++ {
        if points[i].x < min_x {
            min_x = points[i].x
        }
        if points[i].x > max_x {
            max_x = points[i].x
        }
        if points[i].y < min_y {
            min_y = points[i].y
        }
        if points[i].y > max_y {
            max_y = points[i].y
        }
    }

    region_size := 0
    for y := min_y; y <= max_y ; y++ {
        for x := min_x; x < max_x; x++ {
            total_distance := 0.0
            for i := 0; i < n; i++ {
                distance := math.Abs(points[i].x-x)+math.Abs(points[i].y-y)
                total_distance += distance
                if total_distance > MAX_DISTANCE {
                    break
                }
            }
            if total_distance < MAX_DISTANCE {
                region_size++
            }
        }
    }

    return region_size
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")
    n := len(lines)

    points := make([]point, n)
    for i := 0; i < n; i++ {
        fmt.Sscanf(lines[i], "%f, %f", &points[i].x, &points[i].y)
    }

    fmt.Println(part1(points, n))
    fmt.Println(part2(points, n))
}
