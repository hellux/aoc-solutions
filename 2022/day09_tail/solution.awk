BEGIN {
    part1 = "sort -u | wc -l"
    X=0; Y=0
}

function abs(xx) { if (xx < 0) return -xx; else return xx; }

/./ {
    d=$1; n=$2

    for (i = 1; i <= n; i++) {
        if (d == "R") x += 1
        if (d == "U") y += 1
        if (d == "L") x -= 1
        if (d == "D") y -= 1

        if (y == Y) {
            if (X - x > 1) X -= 1
            if (x - X > 1) X += 1
        } else if (x == X) {
            if (Y - y > 1) Y -= 1
            if (y - Y > 1) Y += 1
        } else if (abs(x-X) > 1 || abs(y-Y) > 1) {
            if (X > x) X -= 1
            if (X < x) X += 1
            if (Y > y) Y -= 1
            if (Y < y) Y += 1
        }

        print X, Y | part1
    }
}
