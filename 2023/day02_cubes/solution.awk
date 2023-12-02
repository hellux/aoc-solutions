BEGIN {
    cube_count["red"]=12
    cube_count["green"]=13
    cube_count["blue"]=14
}

{
    gsub("Game ", "")
    split($0, a, ": "); id=a[1]
    ncols = split(a[2], colors, "(;|,) ")

    possible=1
    max["red"]=0;
    max["green"]=0
    max["blue"]=0
    for (i = 1; i <= ncols; i++) {
        split(colors[i], a, " "); n=a[1]; color=a[2]
        if (n > cube_count[color]) possible=0
        if (n > max[color]) max[color]=n
    }

    if (possible) p1 += id
    p2 += max["red"] * max["green"] * max["blue"]
}

END {
    print p1
    print p2
}
