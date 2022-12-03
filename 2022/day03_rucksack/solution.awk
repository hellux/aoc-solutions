BEGIN {
    alpha="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
}
{
    l=length($0)
    r0=substr($0, 1, l/2)
    r1=substr($0, l/2+1)
    for (p = 1; p <= length(alpha); p++) {
        a=substr(alpha, p, 1)
        if (index(r0, a) > 0 && index(r1, a) > 0) {
            part1+=p
            break
        }
    }
}

END {
    print part1
}
