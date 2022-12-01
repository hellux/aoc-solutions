$0 == "" { elf+=1 }
{ cal[elf]+=$0 }
END {
    part1=0
    for (i = 1; i <= elf; i++) if (cal[i] > part1) part1=cal[i]
    print part1
}
