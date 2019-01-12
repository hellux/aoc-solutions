package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "sort"
    "strconv"
)

type guard struct {
    time_asleep int
    sleep_times [60]int
}

func part1(guards map[int]guard) int {
    max_asleep := 0
    max_guard := 0
    for id, guard := range guards {
        if guard.time_asleep > max_asleep {
            max_asleep = guard.time_asleep
            max_guard = id
        }
    }

    max_sleep_time := 0
    max_minute := 0
    sleep_times := guards[max_guard].sleep_times
    for m := 0; m < 60; m++ {
        if sleep_times[m] > max_sleep_time {
            max_sleep_time = sleep_times[m]
            max_minute = m
        }
    }

    return max_guard*max_minute
}

func part2(guards map[int]guard) int {
    max_sleep_time := 0
    max_minute := 0
    max_guard := 0
    for id, guard := range guards {
        for m := 0; m < 60; m++ {
            if guard.sleep_times[m] > max_sleep_time {
                max_sleep_time = guard.sleep_times[m]
                max_minute = m
                max_guard = id
            }
        }
    }

    return max_guard*max_minute
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    log := strings.Split(string(bytes), "\n")
    n := len(log)
    sort.Strings(log)

    guards := make(map[int]guard)
    current_guard := 0
    sleep_start := 0
    for i := 0; i < n; i++ {
        minute, _ := strconv.Atoi(log[i][15:17])
        description := log[i][19:]

        switch description[0] {
        case 'G':
            fmt.Sscanf(description, "Guard #%d begins shift", &current_guard)
        case 'f':
            sleep_start = minute
        case 'w':
            g := guards[current_guard]
            g.time_asleep += minute-sleep_start
            for m := sleep_start; m < minute; m++ {
                g.sleep_times[m]++
            }
            guards[current_guard] = g
        }
    }

    fmt.Println(part1(guards))
    fmt.Println(part2(guards))
}
