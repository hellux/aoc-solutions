BEGIN { N=0 }

/./ { ns[N]=N; vs[N]=$0; N+=1 }

function mix() {
    for (i=0; i<N; i++) {
        for (j=0; j<N; j++) if (ns[j]==i) { p=j; break }
        n=ns[p]
        v=vs[n]

        if (v<0) {
            for (j=1; j<=-v; j++) {
                pn=(p-1+N)%N
                ns[p]=ns[pn]
                p=pn
            }
        } else {
            for (j=1; j<=v; j++) {
                pn=(p+1)%N
                ns[p]=ns[pn]
                p=pn
            }
        }

        ns[p]=n
    }
}

function coords() {
    for (j=0; j<N; j++) if (vs[ns[j]]==0) z=j
    return vs[ns[(z+1000)%N]]+vs[ns[(z+2000)%N]]+vs[ns[(z+3000)%N]]
}

END {
    mix()
    print coords()
}
