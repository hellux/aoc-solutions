$1 == "seeds:" {
    for (i = 2; i <= NF; i++) s[i] = $i
    next
}

$2 == "map:" {
    delete t
    next
}

/./ {
    dst=$1; src=$2; n=$3; diff=dst-src

    for (i in s) {
        if (t[i]) continue
        if (src <= s[i] && s[i] <= src+n) {
            s[i] += diff
            t[i]=1
        }
    }
}

END {
    part1=1e100
    for (i in s) if (s[i]<part1) part1=s[i]
    print part1
}
