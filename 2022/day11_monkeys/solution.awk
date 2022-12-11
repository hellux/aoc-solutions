BEGIN { FS=" " }

/Monkey/ { m=$2+0 }
/Starting/ {
    for (i = 3; i <= NF; i++) {
        items[m, i-2] = $i+0
        ic[m] += 1
    }
}
/Operation/ { op[m] = $5; right[m] = $6 }
/Test/ { div[m] = $4 }
/true/ { true[m] = $6 }
/false/ { false[m] = $6 }

function rounds(R) {
    for (r = 1; r <= R; r++) {
        for (m = 0; m <= M; m++) {
            for (i = 1; i <= ic[m]; i++) {
                lhs=items[m, i];
                rhs=right[m]; if (rhs == "old") rhs=items[m, i]

                if (op[m] == "+") new=lhs+rhs
                else if (op[m] == "*") new=lhs*rhs

                new=int(new/3)

                if (new % div[m] == 0) receiver=true[m]
                else receiver=false[m]

                ic[receiver]+=1
                items[receiver, ic[receiver]]=new
            }
            inspect[m]+=ic[m]
            ic[m]=0
        }
    }
}

END {
    M=m

    rounds(20)
    part1="sort -n | tail -n2 | awk 'BEGIN {x=1} {x=x*$0} END {print x}'"
    for (m = 0; m <= M; m++) print inspect[m] | part1
    close(part1)
}
