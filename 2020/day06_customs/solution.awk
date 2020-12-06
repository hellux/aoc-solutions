function count_group() {
    for (i in g) {
        part1 += 1
        if (g[i] == n) part2 += 1
    }
}

NF > 0 { for (i = 1; i <= length($0); ++i) { g[substr($0,i,1)] += 1 }; n += 1 }
NF == 0 { count_group(); delete g; n = 0 }
END { count_group(); print part1 "\n" part2 }
