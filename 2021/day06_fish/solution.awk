BEGIN { RS="," }
{ t[NR] = 0 + $0 }
END {
    for (i = 1; i <= 80; i++)
        for (f in t) {
            t[f]--
            if (t[f] == -1) {
                t[f] = 6
                t[length(t)+1] = 8
            }
        }
    print length(t)
}
