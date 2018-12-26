package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
)

const CHAR_PLANT = '#'
const CHAR_EMPTY = '.'

func evolve(plants map[int]bool, rules [32]bool) (map[int]bool, uint64) {
    /* find min and max positions of plants */
    min := int(^uint(0) >> 1)
    max := -min - 1
    for pos, plant := range plants {
        if plant {
            if pos < min {
                min = pos
            } else if pos > max {
                max = pos
            }
        }
    }

    /* generate next generation */
    nextgen := make(map[int]bool)
    pattern := 0
    signature := uint64(0)
    for i := min-2; i <= max+2; i++ {
        /* shift and update pattern */
        pattern <<= 1
        if plants[i+2] {
            pattern ^= 1
        }
        pattern &= 0x1f

        /* insert plant */
        if rules[pattern] {
            signature ^= (1 << uint(i-(min-2)))
            nextgen[i] = true
        } else {
            nextgen[i] = false
        }
    }

    return nextgen, signature
}

const PART1_GEN = 20
const PART2_GEN = 50e9

func part1(plants map[int]bool, rules [32]bool) int {
    for i := 0; i < PART1_GEN; i++ {
        plants, _ = evolve(plants, rules)
    }
    sum := 0
    for pos, plant := range plants {
        if plant {
            sum += pos
        }
    }
    return sum
}

func part2(plants map[int]bool, rules [32]bool) int {
    gen := 1
    sig_prev := uint64(0)
    sum_prev := 0
    for {
        var sig uint64
        plants, sig = evolve(plants, rules)
        sum := 0
        for pos, plant := range plants {
            if plant {
                sum += pos
            }
        }
        if sig == sig_prev {
            return sum + (sum-sum_prev)*(PART2_GEN-gen)
        } else {
            sum_prev = sum
            sig_prev = sig
            gen += 1
        }
    }
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")

    plants := make(map[int]bool)
    var rules [32]bool

    var init_str string
    fmt.Sscanf(lines[0], "initial state: %s\n", &init_str)
    for i, char := range init_str {
        if char == CHAR_PLANT {
            plants[i] = true
        }
    }

    for _, line := range lines[2:] {
        pattern := 0
        for i := 0; i < 5; i++ {
            if line[i] == CHAR_PLANT {
                pattern ^= (1 << uint(4-i))
            }
        }
        if line[9] == CHAR_PLANT {
            rules[pattern] = true
        }
    }

    fmt.Println(part1(plants, rules))
    fmt.Println(part2(plants, rules))
}
