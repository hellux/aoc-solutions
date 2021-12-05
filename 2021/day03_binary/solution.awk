BEGIN { FS="" }
{
    for (i = 1; i <= NF; i++)
        if ($i == "1") dir[i] += 1
        else dir[i] -= 1
}
END {
    for (i = 1; i <= NF; i++)
        if (dir[i] > 0) gamma += 2^(NF-i)
    print gamma * (2^NF - gamma - 1)
}
