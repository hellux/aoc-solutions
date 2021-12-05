function b(s) {
    x = 0; for (i = 1; i <= NF; i++) x += 2^(NF-i) * substr(s, i, 1)
    return x
}

function mc(a, _i) {
    d = 0; for (j in a) if (substr(a[j], _i, 1) == "1") d += 1; else d -= 1
    return d >= 0
}

BEGIN { FS="" }

{ o2p[NR] = $0; co2p[NR] = $0 }

END {
    for (i = 1; i <= NF; i++) {
        c = mc(o2p, i); gamma = gamma c; epsilon = epsilon 1-c
    }

    for (i = 1; i <= NF; i++) {
        c = mc(o2p, i)
        if (length(o2p) > 1)
            for (j in o2p) if (substr(o2p[j], i, 1) != c) delete o2p[j]
        for (j in o2p) { o2 = o2p[j] }

        u = 1 - mc(co2p, i)
        if (length(co2p) > 1)
            for (j in co2p) if (substr(co2p[j], i, 1) != u) delete co2p[j]
        for (j in co2p) { co2 = co2p[j] }
     }

    print b(gamma) * b(epsilon)
    print b(o2) * b(co2)
}
