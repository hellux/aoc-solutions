BEGIN {
    part1 = "sort -u | wc -l"
    part2 = "sort -u | wc -l "  # use space to differentiate, need two in parallel
    T=9
    for (t = 0; t <= T; t++) { x[t]=0; y[t]=0 }
}

function abs(xx) { if (xx < 0) return -xx; else return xx; }

/./ {
    d=$1; n=$2

    for (i = 1; i <= n; i++) {
        if (d == "R") x[0] += 1
        if (d == "U") y[0] += 1
        if (d == "L") x[0] -= 1
        if (d == "D") y[0] -= 1

        for (t = 1; t <= T; t++) {
            h=t-1

            if (y[t] == y[h]) {
                if (x[t] - x[h] > 1) x[t] -= 1
                if (x[h] - x[t] > 1) x[t] += 1
            } else if (x[t] == x[h]) {
                if (y[t] - y[h] > 1) y[t] -= 1
                if (y[h] - y[t] > 1) y[t] += 1
            } else if (abs(x[h]-x[t]) > 1 || abs(y[h]-y[t]) > 1) {
                if (x[t] > x[h]) x[t] -= 1
                if (x[t] < x[h]) x[t] += 1
                if (y[t] > y[h]) y[t] -= 1
                if (y[t] < y[h]) y[t] += 1
            }
        }

        print x[1], y[1] | part1
        print x[T], y[T] | part2
    }
}

END { close(part1); close(part2) }
