package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "strconv"
)

func part1(changes []int, n int) int {
    freq := 0
    for i := 0; i < n; i++ {
        freq += changes[i]
    }
    return freq
}

type tree struct {
    left *tree
    value int
    right *tree
}

func part2(changes []int, n int) int {
    freq := 0
    previous := &tree{nil, freq, nil}
    for {
        for i := 0; i < n; i++ {
            freq += changes[i]
            current := previous
            for {
                if freq == current.value {
                    return freq
                } else if freq < current.value {
                    if current.left == nil {
                        current.left = &tree{nil, freq, nil}
                        break
                    } else {
                        current = current.left
                    }
                } else {
                    if current.right == nil {
                        current.right = &tree{nil, freq, nil}
                        break
                    } else {
                        current = current.right
                    }
                }
            }
        }
    }
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), " ")
    n := len(lines)

    changes := make([]int, n)
    for i := 0; i < n; i++ {
        change, _ := strconv.Atoi(lines[i])
        changes[i] = change
    }

    fmt.Println(part1(changes, n))
    fmt.Println(part2(changes, n))
}
