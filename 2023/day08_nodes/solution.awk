NR==1 { ins=$0 }

/=/ {
    if (substr($1, 3, 1)=="A") ns[++m]=$1
    s[$1,"L"]=substr($3, 2, 3);
    s[$1,"R"]=substr($4, 1, 3);
}

function gcd(a, b) {
    if (a<b) return a != 0 ? gcd(a, b%a) : b
    else if (b<a) return b != 0 ? gcd(a%b, b) : a
    else return a
}

function lcm(a, b) { return a * b / gcd(a, b); }

END {
    n="AAA"
    while (n SUBSEP "L" in s) {
        i=substr(ins, part1%(length(ins))+1, 1)
        part1+=1
        n=s[n, i]
        if (n=="ZZZ") break
    }
    print part1

    while (1) {
        i=substr(ins, k%(length(ins))+1, 1)
        k+=1

        end=1
        for (j=1; j<=m; j++) {
            ns[j]=s[ns[j], i]
            if (!cs[j]) {
                if (substr(ns[j], 3, 1) == "Z") cs[j]=k
                end=0
            }
        }
        if (end) break
    }
    part2=1
    for (j=1; j<=m; j++) part2=lcm(part2, cs[j])
    print part2
}
