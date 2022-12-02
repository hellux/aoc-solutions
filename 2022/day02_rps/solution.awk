function score(aa, bb) {
    s=aton(bb)
    if (winner(aa, bb) == -1) return s
    if (winner(aa, bb) == 0) return s+3
    if (winner(aa, bb) == 1) return s+6
}
function aton(x) {
    if (x == "A") return 1;
    if (x == "B") return 2;
    if (x == "C") return 3;
    if (x == "X") return 1;
    if (x == "Y") return 2;
    if (x == "Z") return 3;
}
function winner(aa, bb) {
    if (aa == bb) return 0

    if (aa == "A" && bb == "Y") return 1
    if (aa == "A" && bb == "Z") return -1

    if (aa == "B" && bb == "Z") return 1
    if (aa == "B" && bb == "X") return -1

    if (aa == "C" && bb == "X") return 1
    if (aa == "C" && bb == "Y") return -1
}
BEGIN { FS = " " }
{
    part1+=score($1, $2)
}
END {
    print part1
}
