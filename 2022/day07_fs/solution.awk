BEGIN { FS=" " }

/\$ cd .+/ {
    if ($3 == "..") {
        d -= 1
    } else {
        d += 1
        s[d] = $3
    }
}

/[0-9]+ .+/ {
    for (i = 1; i <= d; i++) {
        path=""
        for (j = 1; j <= i; j++) path=path "/" s[j]
        sizes[path] += $1
    }
}

END {
    total=70000000
    needed=30000000
    used=sizes["//"]
    unused=total-used
    required=needed-unused

    part1=0
    part2=used
    for (a in sizes) {
        if (sizes[a] <= 100000) part1 += sizes[a]
        if (sizes[a] >= required && sizes[a] < part2) part2 = sizes[a]
    }
    print part1
    print part2
}
