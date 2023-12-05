function swap() {
    # add identity mappings for unmapped gaps
    delete ru
    for (i in r) {
        i=+i

        # collect and sort overlapping
        nw=0
        for (j in rn) if (i <= +j && +j < r[i]) wu[++nw]=j
        do {
            unsorted=0
            for (w=1; w<nw; w++) {
                if (wu[w]>wu[w+1]) {
                    tmp=wu[w]
                    wu[w]=wu[w+1]
                    wu[w+1]=tmp
                    unsorted=1
                }
            }
        } while (unsorted)

        b_prev=i
        for (w=1; w<=nw; w++) {
            a=wu[w]; b=rn[a]
            if (b_prev<a) ru[b_prev]=a
            b_prev=b
        }
        if (b_prev<r[i]) ru[b_prev]=r[i]
    }

    delete r
    for (i in rn) r[i+rdn[i]]=rn[i]+rdn[i]
    for (i in ru) r[i]=ru[i]

    delete rn
    delete rdn
}

$1 == "seeds:" {
    for (i = 2; i <= NF; i++) s[i] = $i
    for (i = 2; i <= NF; i+=2) {
        rn[$i] = $i+$(i+1)
        rdn[$i] = 0
    }
    next
}

$2 == "map:" {
    delete t
    swap()
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

    for (rs in r) {
        if (+rs < src) a=src; else a=rs
        if (r[rs] < src+n) b=r[rs]; else b=src+n
        if (a<b) {
            rn[a]=b;
            rdn[a]=diff
        }
    }
}

END {
    part1=1e100
    for (i in s) if (s[i]<part1) part1=s[i]
    print part1

    swap()
    part2=1e100
    for (i in r) { if (+i<part2) part2=i }
    print part2
}
