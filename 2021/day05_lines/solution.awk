{
    split($1, a, ","); x1 = a[1]; y1 = a[2]
    split($3, a, ","); x2 = a[1]; y2 = a[2]
    if (x1 == x2) {
        if (y1 > y2) { tmp = y1; y1 = y2; y2 = tmp }
        for (y = y1; y <= y2; y++) { w1[x1, y]++; w2[x1, y]++ }
    } else if (y1 == y2) {
        if (x1 > x2) { tmp = x1; x1 = x2; x2 = tmp }
        for (x = x1; x <= x2; x++) { w1[x, y1]++; w2[x, y1]++ }
    } else {
        if (y1 > y2) {
            tmp = y1; y1 = y2; y2 = tmp
            tmp = x1; x1 = x2; x2 = tmp
        }
        x = x1
        for (y = y1; y <= y2; y++) {
            w2[x, y]++
            if (x1 < x2) x++
            else x--
        }
    }
}
END {
    for (i in w1) if (w1[i] > 1) part1++; print part1
    for (i in w2) if (w2[i] > 1) part2++; print part2
}
