BEGIN {
    FS=","
    for (i=0; i<256; i++) ord[sprintf("%c", i)]=i
}
{
    for (f=1; f<=NF; f++) {
        v=0
        for (i=1; i<=length($f); i++) {
            v+=ord[substr($f, i, 1)]
            v*=17
            v%=256
        }
        part1+=v
    }
}
END {
    print part1
}
