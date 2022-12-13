BEGIN { RS="\n\n"; FS="\n" }

function tok(x) {
    if (x~/^[\[\],]/) { skip=2; return substr(x, 1, 1) }
    else { skip=match(x, /.[,\]]/)+1; return substr(x, 1, skip-1) }
}

function cmp(l, r) {
    lt=tok(l); l=substr(l, skip)
    rt=tok(r); r=substr(r, skip)
    if (lt~/[0-9]/ && rt~/[0-9]/ && int(lt) < int(rt)) return 1
    if (lt~/[0-9]/ && rt~/[0-9]/ && int(lt) > int(rt)) return 0
    else if (lt=="]" && rt~/,|[0-9]/) return 1
    else if (lt~/,|[0-9]/ && rt=="]") return 0
    else if (lt=="]" && rt=="[") return 1
    else if (lt=="[" && rt=="]") return 0
    else if (lt=="[" && rt~/[0-9]/) {
        l="[" l
        r="[" rt "]" r
    } else if (lt~/[0-9]/ && rt=="[") {
        l="[" lt "]" l
        r="[" r
    }
    return cmp(l, r)
}

{
    if (cmp($1, $2)) part1+=NR
}

END {
    print part1
}
