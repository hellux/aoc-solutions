BEGIN {
    FS=" "
    c=1; x=1
}

function cycle() {
    if (C == c) return
    C=c

    if ((c+20)%40 == 0) part1+=c*x
}

{ cycle() }
{ c+=1 }
{ cycle() }
/addx/ { c+=1; x+=$2 }

END {
    cycle()

    print part1
}
