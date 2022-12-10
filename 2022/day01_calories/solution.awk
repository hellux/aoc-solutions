BEGIN { part2="sort -n | tail -n3 | awk '{s+=$0} END {print s}'" }
{ sum += $0 }
/^$/ {
    if (sum > part1) part1=sum
    print sum | part2
    sum=0
}
END { print part1; close(part2) }
