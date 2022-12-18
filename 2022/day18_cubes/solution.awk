BEGIN {
    FS=","
    sides[1]=" 0,0,1"; sides[2]="0, 1,0"; sides[3]="1,0, 0"
    sides[4]="-1,0,0"; sides[5]="0,-1,0"; sides[6]="0,0,-1"
}

/./ {
    c[$1,$2,$3]=1
}

END {
    part1=0
    for (i in c) {
        split(i, p, SUBSEP)
        for (s in sides) {
            split(sides[s], d, ",")
            px=p[1]+d[1]
            py=p[2]+d[2]
            pz=p[3]+d[3]
            if (!c[px,py,pz]) {
                cf[px,py,pz]=1
                part1++
            }
        }
    }
    print part1
}
