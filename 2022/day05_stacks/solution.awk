BEGIN { N=9 }

function revert(x) {
    y=""
    for (ii = 0; ii < length(x); ii++) y=y substr(x, length(x)-ii, 1)
    return y
}

/^.*\[/ {
    for (i = 1; i <= N; i++) {
        l = substr($0, 4*(i-1) + 2, 1);
        if (l != " ") s[i] = s[i] l
    }

    next
}
/^move/ {
    n=$2
    from=$4
    to=$6

    for (i = 1; i <= n; i++) {
        l=substr(s[from], length(s[from]), 1)
        if (l != "") {
            s[from]=substr(s[from], 1, length(s[from])-1)
            s[to]=s[to] l
        }
    }

    l=substr(s2[from], length(s2[from])-n+1, n)
    if (l != "") {
        s2[from]=substr(s2[from], 1, length(s2[from])-n)
        s2[to]=s2[to] l
    }

    next
}
/^$/ {
    for (i = 1; i <= N; i++) {
        s[i] = revert(s[i])
        s2[i] = s[i]
    }
}

END {
    for (i = 1; i <= N; i++) printf "%s", substr(s[i], length(s[i]), 1)
    print ""

    for (i = 1; i <= N; i++) printf "%s", substr(s2[i], length(s2[i]), 1)
    print ""
}
