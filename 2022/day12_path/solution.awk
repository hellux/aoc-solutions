BEGIN {
    alpha="abcdefghijklmnopqrstuvwxyz"
    inf=1000000000
}

/./ {
    W=length($0)
    for (x = 1; x <= W; x++) {
        c=substr($0, x, 1)
        N+=1
        if (c == "S") { sn=N; c="a" }
        else if (c == "E") { en=N; c="z" }
        h[N]=index(alpha, c)
    }
}

function distances(start, reverse) {
    delete unvis; delete dist
    for (n in h) { unvis[n]=1; dist[n]=inf }
    dist[start]=0

    while (length(unvis)) {
        min=inf; cn=0
        for (i in unvis) if (unvis[i] && dist[i]<min) { min=dist[i]; cn=i }
        if (!cn) break

        ns[1]=-1; ns[2]=1; ns[3]=W; ns[4]=-W
        for (ni=1; ni<=4; ni++) {
            nn=cn+ns[ni]
            if (nn < 1 || nn > N) continue
            if (!nn in unvis) continue
            hd=h[nn]-h[cn]
            if (reverse) hd=-hd
            if (hd > 1) continue
            else d=dist[cn]+1
            if (dist[nn] > d) dist[nn]=d
        }

        delete unvis[cn]
    }
}

END {
    distances(sn)
    print dist[en]

    distances(en, 1)
    part2=inf
    for (n in h) if (h[n]==1 && dist[n]<part2) part2=dist[n]
    print part2
}
