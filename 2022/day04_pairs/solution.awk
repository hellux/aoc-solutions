{
    split($0, aa, ","); left=aa[1]; right=aa[2];
    split(left, bb, "-"); a0=bb[1]; a1=bb[2];
    split(right, cc, "-"); b0=cc[1]; b1=cc[2]

    if ((a0 <= b0 && a1 >= b1) || (b0 <= a0 && b1 >= a1)) part1 += 1
    if ((a0 <= b0 && b0 <= a1) || (b0 <= a0 && a0 <= b1)) part2 += 1
}

END {
    print part1
    print part2
}
