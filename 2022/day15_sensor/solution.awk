BEGIN {
    PY=2000000
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
}

END {
    print length(part1)
}
