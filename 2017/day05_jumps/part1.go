package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "strconv"
)

func part1(jumps []int, n int) int {
    i := 0
    jumpc := 0
    for 0 <= i && i < n {
        jump := jumps[i]
        jumps[i]++
        i += jump
        jumpc++
    }

    return jumpc
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")
    n := len(lines)

    jumps := make([]int, n)
    for i := 0; i < n; i++ {
        jump, _ := strconv.Atoi(lines[i])
        jumps[i] = jump
    }

    fmt.Println(part1(jumps, n))
}
