BEGIN { RS="m"; FS=")" }

$1 ~ /ul\([0-9]+,[0-9]+$/ {
    nums=substr($1, 4)
    split(nums, ps, ",")
    p1+=ps[1]*ps[2]
}

END { print p1 }
