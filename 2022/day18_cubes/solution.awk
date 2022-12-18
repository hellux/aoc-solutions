BEGIN {
    FS=","
    sides[1]=" 0,0,1"; sides[2]="0, 1,0"; sides[3]="1,0, 0"
    sides[4]="-1,0,0"; sides[5]="0,-1,0"; sides[6]="0,0,-1"
}

/./ {
    c[$1,$2,$3]=1
    if ($1<xmin) xmin=$1; if ($2<ymin) ymin=$2; if ($3<zmin) zmin=$3
    if ($1>xmax) xmax=$1; if ($2>ymax) ymax=$2; if ($3>zmax) zmax=$3
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

    # find all outer cubes of air
    co[xmin-1,ymin,zmin]=1
    while (!done) {
        done=1
        for (i in co) {
            if (!co[i]) continue
            split(i, p, SUBSEP)
            for (s in sides) {
                split(sides[s], d, ",")
                px=p[1]+d[1]
                py=p[2]+d[2]
                pz=p[3]+d[3]
                if (!c[px,py,pz] && !co[px,py,pz] &&
                    xmin-1<=px && px<=xmax+1 &&
                    ymin-1<=py && py<=ymax+1 &&
                    zmin-1<=pz && pz<=zmax+1) {
                    co[px,py,pz]=1
                    done=0
                }
            }
        }
    }

    # subtract inner surfaces
    part2=part1
    for (i in cf) {
        if (!cf[i] || co[i]) continue
        split(i, p, SUBSEP)
        for (s in sides) {
            split(sides[s], d, ",")
            px=p[1]+d[1]
            py=p[2]+d[2]
            pz=p[3]+d[3]
            if (c[px,py,pz]) part2--
        }
    }
    print part2
}
