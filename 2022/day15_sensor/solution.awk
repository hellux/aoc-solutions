BEGIN {
    PY=2000000
    MAX=2*PY
}

function abs(xxx) { if (xxx < 0) return -xxx; else return xxx }
function md(x0, y0, x1, y1) { return abs(x0-x1)+abs(y0-y1) }

/./ {
    split( $3, p, "="); sx=int(p[2])
    split( $4, p, "="); sy=int(p[2])
    split( $9, p, "="); bx=int(p[2])
    split($10, p, "="); by=int(p[2])

    d=md(sx, sy, bx, by)

    if (sy-d <= PY && PY <= sy+d) {
        n=d-abs(PY-sy)
        for (x=sx-n; x<=sx+n; x++) part1[x]=1
    }
    if (by==PY) delete part1[bx]

    n=0
    for (y=d+1; y>=0; y--) {
        yy=sy-y
        if (0 <= yy && yy <= MAX) {
            xx=sx-n; if (0 <= xx && xx <= MAX) w[xx,yy]=1
            if (n>0)
            xx=sx+n; if (0 <= xx && xx <= MAX) w[xx,yy]=1
        }
        if (y==0) continue
        yy=sy+y
        if (0 <= yy && yy <= MAX) {
            xx=sx-n; if (0 <= xx && xx <= MAX) w[xx,yy]=1
            if (n>0)
            xx=sx+n; if (0 <= xx && xx <= MAX) w[xx,yy]=1
        }
        n+=1
    }
    N+=1
    sxs[N]=sx; sys[N]=sy
    mds[N]=d
}

END {
    print length(part1)

    for (i in w) {
        split(i, p, SUBSEP); x=p[1]; y=p[2]
        outside=1
        for (i=0; i<=N; i++) {
            d=md(x,y,sxs[i],sys[i])
            if (d <= mds[i]) {
                outside=0
                break
            }
        }
        if (outside) {
            print x*MAX + y
            break
        }
    }
}
