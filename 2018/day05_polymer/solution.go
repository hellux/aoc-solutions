package main

import (
    "io/ioutil"
    "os"
    "fmt"
    "math"
    "strings"
)

const CAP_DIFF = 'a'-'A'

func react(polymer string) string {
    changed := true
    for changed {
        changed = false
        i := 0
        for i < len(polymer)-1 {
            diff := math.Abs(float64(polymer[i])-float64(polymer[i+1]))
            if (diff == CAP_DIFF) {
                polymer = string(append([]byte(polymer[:i]), polymer[i+2:]...))
                changed = true
            } else {
                i++
            }
        }
    }
    return polymer
}

func part1(polymer string) int {
    return len(react(polymer))
}

func part2(polymer string) int {
    min_length := len(polymer)
    for i := 'A'; i <= 'Z'; i++ {
        upper := string(i)
        lower := string(i+CAP_DIFF)
        rm_upper := strings.Replace(polymer, upper, "", -1)
        rm_lower := strings.Replace(rm_upper, lower, "", -1)
        if len(rm_lower) < len(polymer) {
            length := len(react(rm_lower))
            if length < min_length {
                min_length = length
            }
        }
    }
    return min_length
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    polymer := string(bytes)

    fmt.Println(part1(polymer))
    fmt.Println(part2(polymer))
}
