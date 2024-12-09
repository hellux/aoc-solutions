BEGIN { RS="m"; FS=")"; m=1 }

$1 ~ /ul\([0-9]+,[0-9]+$/ {
    nums=substr($1, 4)
    split(nums, ps, ",")
    p1+=ps[1]*ps[2]
    if (m) p2+=ps[1]*ps[2]
}

{
    s=$0
    while (match(s, /do\(\)|don't\(\)/)) {
        cmd=substr(s,RSTART,RLENGTH)
        s=substr(s,RSTART+RLENGTH)
        m=cmd=="do()"
    }
}

END {
    print p1
    print p2
}
