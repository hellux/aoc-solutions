function find_marker(n) {
    m=length($0)
    for (i = n; i <= m; i++) {
        q=substr($0, i-n+1, n)
        dup=0
        for (j = 1; j <= n; j++) {
            for (k = 1; k <= n; k++) {
                if (j == k) break
                if (substr(q, k, 1) == substr(q, j, 1)) {
                    dup=1
                    break
                }
            }
        }
        if (!dup) return i;
    }
}

{
    print find_marker(4);
    print find_marker(14);
}
