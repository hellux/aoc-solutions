package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
)

func part1(prereqs [256][]byte, n int) string {
    steps := make([]byte, n, n)
    for n > 0 {
        for c := 'A'; c <= 'Z'; c++ {
            ready := true
            if prereqs[c] == nil {
                ready = false
            } else if len(prereqs[c]) > 0 {
                for _, p := range prereqs[c] {
                    if prereqs[p] != nil {
                        ready = false
                        break
                    }
                }
            }
            if ready {
                prereqs[c] = nil
                steps = append(steps, byte(c))
                n--
                break
            }
        }
    }
    return string(steps)
}

const WORKERS = 15
const TASK_DELAY = 60

func part2(prereqs [256][]byte, n int) int {
    steps := make([]rune, WORKERS, WORKERS) /* step worker is working on */
    times := make([]int, WORKERS, WORKERS) /* remaining time workers step */

    total_time := 0
    for n > 0 {
        fast_forward := true

        /* begin with new steps */
        for c := 'A'; c <= 'Z'; c++ {
            ready := true /* step has no unfinished prerequisites */
            if prereqs[c] == nil {
                ready = false
            } else if len(prereqs[c]) > 0 {
                for _, p := range prereqs[c] {
                    if prereqs[p] != nil {
                        ready = false
                        break
                    }
                }
            }
            ongoing := false /* step is currently being worked on */
            for w := 0; w < WORKERS; w++ {
                if steps[w] == c {
                    ongoing = true
                    break
                }
            }
            if ready && !ongoing {
                /* find available worker */
                new_worker := -1
                for w := 0; w < WORKERS; w++ {
                    if times[w] == 0 {
                        new_worker = w
                        break
                    }
                }

                /* let worker begin with step */
                if (new_worker >= 0) {
                    time := TASK_DELAY + int(c-'A'+1)
                    times[new_worker] = time
                    steps[new_worker] = c
                    fast_forward = false
                }
            }
        }

        if fast_forward {
            /* calc time to jump */
            min_time := 0
            for w := 0; w < WORKERS; w++ {
                if min_time == 0 {
                    min_time = times[w]
                } else if times[w] > 0 && times[w] < min_time {
                    min_time = times[w]
                }
            }
            /* fast forward */
            for w := 0; w < WORKERS; w++ {
                if times[w] > 0 {
                    times[w] -= min_time
                    if times[w] == 0 {
                        n--
                        prereqs[steps[w]] = nil
                    }
                }
            }
            total_time += min_time
        }

    }
    return total_time
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")

    var prereqs [256][]byte
    n := 0
    for i := 0; i < len(lines); i++ {
        before := byte(0); after := byte(0)
        fmt.Sscanf(lines[i],
                   "Step %c must be finished before step %c can begin.",
                   &before, &after)
        if prereqs[before] == nil {
            prereqs[before] = make([]byte, 0, 10)
            n++
        }
        if prereqs[after] == nil {
            prereqs[after] = make([]byte, 0, 10)
            n++
        }
        prereqs[after] = append(prereqs[after], before)
    }

    fmt.Println(part1(prereqs, n))
    fmt.Println(part2(prereqs, n))
}
