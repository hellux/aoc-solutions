BEGIN { prev=-1; N=3 }
{
    for (i = N; i >= 2; i--) window[i] = window[i-1]
    window[1] = $0

    sum_prev = sum; sum = 0
    for (i = 1; i <= N; i++) sum += window[i]

    if (NR > 1 && window[1] > window[2]) part1++
    if (NR > 3 && sum > sum_prev) part2++
}
END { print part1; print part2 }
