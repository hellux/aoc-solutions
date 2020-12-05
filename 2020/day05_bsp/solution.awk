BEGIN { part1 = 0 }
{
    id = 0;
    for (i = 1; i <= 10; ++i) {
        if (substr($0, i, 1) ~ /[BR]/)
            id += 2^(10-i);
    }
    ids[id] = 1;

    if (id > part1) part1 = id
}
END {
    for (id = 1; id < part1; ++id)
        if (!(id in ids) && id-1 in ids && id+1 in ids)
            part2 = id

    print part1 "\n" part2
}
