package main

import (
    "io/ioutil"
    "os"
    "fmt"
)

type marble struct {
    counter_clockwise *marble
    number int
    clockwise *marble
}

func part12(playerc int, marblec int) int {
    scores := make([]int, playerc, playerc)
    current := &marble{nil, 0, nil}
    current.clockwise = current
    current.counter_clockwise = current
    player := 0
    high_score := 0
    for m := 1; m <= marblec; m++ {
        if m % 23 == 0 {
            remove := current
            for i := 0; i < 7; i++ {
                remove = remove.counter_clockwise
            }
            remove.clockwise.counter_clockwise = remove.counter_clockwise
            remove.counter_clockwise.clockwise = remove.clockwise
            scores[player] += remove.number + m

            if scores[player] > high_score {
                high_score = scores[player]
            }
            current = remove.clockwise
        } else {
            /* insert marble in circle */
            before := current.clockwise
            after := current.clockwise.clockwise
            current = &marble{before, m, after}
            before.clockwise = current
            after.counter_clockwise = current
        }
        player = (player+1) % playerc
    }

    return high_score
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    player_count := 0
    last_worth := 0
    fmt.Sscanf(string(bytes), "%d players; last marble is worth %d points\n",
               &player_count, &last_worth)

    fmt.Println(part12(player_count, last_worth))
    fmt.Println(part12(player_count, last_worth*100))
}
