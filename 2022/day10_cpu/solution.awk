BEGIN {
    FS=" "
    c=1; x=1
    W=40; H=6
    part2="cat"
}

function cycle() {
    if (C == c) return
    C=c

    if ((c+20)%40 == 0) part1+=c*x

    if (c <= W*H) {
        p=(c-1)%W
        if (x-1 <= p && p <= x+1) printf "#" | part2
        else printf "." | part2
        if (p == W-1) printf "\n" | part2
    }
}

{ cycle() }
{ c+=1 }
{ cycle() }
/addx/ { c+=1; x+=$2 }

END {
    cycle()

    print part1
    close(part2)
}
