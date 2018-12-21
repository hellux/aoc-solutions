package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "sort"
)

func part1(ids []string, n int) int {
    letterc := int('z'-'a'+1)
    doublets := 0
    triplets := 0
    for i := 0; i < n; i++ {
        id := ids[i]
        occurences := make([]int, letterc, letterc)
        for c := 0; c < len(id); c++ {
            index := int(id[c]-'a')
            occurences[index]++
        }
        counted_doublet := false
        counted_triplet := false
        for l := 0; l < letterc; l++ {
            if !counted_doublet && occurences[l] == 2 {
                doublets++
                counted_doublet = true
            } else if !counted_triplet && occurences[l] == 3 {
                triplets++
                counted_triplet = true
            }
        }
    }

    return doublets*triplets
}

func part2(ids []string, n int) string {
    sort.Strings(ids)

    for i := 0; i < n-1; i++ {
        current := ids[i]
        next := ids[i+1]
        diffc := 0
        diffi := 0
        for c := 0; c < len(current); c++ {
            if current[c] != next[c] {
                diffi = c
                diffc++
                if diffc > 1 {
                    break
                }
            }
        }

        if diffc == 1 {
            var remaining strings.Builder
            remaining.WriteString(current[:diffi])
            remaining.WriteString(current[diffi+1:])
            return remaining.String()
        }
    }

    return ""
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    ids := strings.Split(string(bytes), " ")
    n := len(ids)

    fmt.Println(part1(ids, n))
    fmt.Println(part2(ids, n))
}
