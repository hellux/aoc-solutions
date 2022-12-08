/./ {
    w=length($0)
    for (i = 1; i <= w; i++) g[i, NR] = substr($0, i, 1)
}
END {
    part1=0
    for (x = 1; x <= w; x++) {
        for (y = 1; y <= w; y++) {
            n=g[x, y]

            u=1; for (Y = w; Y > y; Y--) if (g[x, Y] >= n) { u = 0; break }
            l=1; for (X = w; X > x; X--) if (g[X, y] >= n) { l = 0; break }
            r=1; for (X = 1; X < x; X++) if (g[X, y] >= n) { r = 0; break }
            d=1; for (Y = 1; Y < y; Y++) if (g[x, Y] >= n) { d = 0; break }
            if (r || l || d || u) part1 += 1
        }
    }
    print part1
}
