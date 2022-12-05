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

    next
}
/^$/ {
    for (i = 1; i <= N; i++) {
        s[i] = revert(s[i])
    }
}

END {
    for (i = 1; i <= N; i++) printf "%s", substr(s[i], length(s[i]), 1)
    print ""
}
