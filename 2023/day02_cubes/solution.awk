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
    for (i = 1; i <= ncols; i++) {
        split(colors[i], a, " "); n=a[1]; color=a[2]
        if (n > cube_count[color]) possible=0
    }

    if (possible) p1 += id
}

END {
    print p1
}
