{
    split($1, a, ","); x1 = a[1]; y1 = a[2]
    split($3, a, ","); x2 = a[1]; y2 = a[2]
    if (y1 > y2) { tmp = y1; y1 = y2; y2 = tmp }
    if (x1 > x2) { tmp = x1; x1 = x2; x2 = tmp }
    if (x1 == x2) for (y = y1; y <= y2; y++) world[x1, y]++
    else if (y1 == y2) for (x = x1; x <= x2; x++) world[x, y1]++
}
END {
    for (i in world) if (world[i] > 1) part1++
    print part1
}
