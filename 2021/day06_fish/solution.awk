function sim(n) {
    delete tc; for (f in t) tc[t[f]]++
    for (i = 1; i <= n; i++) {
        delete tc2; for (f in tc)
            if (f == 0) {
                tc2[6] += tc[0]
                tc2[8] += tc[0]
            } else {
                tc2[f-1] += tc[f]
            }
        delete tc; for (f in tc2) tc[f] = tc2[f]
    }
    s = 0; for (f in tc) s += tc[f]; return s
}
BEGIN { RS="," }
{ t[NR] = 0 + $0 }
END {
    print sim(80)
    print sim(256)
}
