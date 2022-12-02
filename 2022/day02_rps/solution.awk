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
function play(aa, cc) {
    if (aa == "A" && cc == "X") return "Z"
    if (aa == "B" && cc == "X") return "X"
    if (aa == "C" && cc == "X") return "Y"

    if (aa == "A" && cc == "Y") return "X"
    if (aa == "B" && cc == "Y") return "Y"
    if (aa == "C" && cc == "Y") return "Z"

    if (aa == "A" && cc == "Z") return "Y"
    if (aa == "B" && cc == "Z") return "Z"
    if (aa == "C" && cc == "Z") return "X"

    return "E"
}
BEGIN { FS = " " }
{
    part1+=score($1, $2)
    part2+=score($1, play($1, $2))
}
END {
    print part1
    print part2
}
