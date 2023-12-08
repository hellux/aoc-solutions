NR==1 { ins=$0 }

/=/ {
    s[$1,"L"]=substr($3, 2, 3);
    s[$1,"R"]=substr($4, 1, 3);
}

END {
    n="AAA"
    while (1) {
        i=substr(ins, part1%(length(ins))+1, 1)
        part1+=1
        n=s[n, i]
        if (n=="ZZZ") break
    }
    print part1
}
