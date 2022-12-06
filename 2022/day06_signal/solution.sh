mark() {
    for i in $(seq $(echo $s | wc -c)); do
        n=$((i + $1 - 1))
        [ $(echo $s | cut -c$i-$n | sed 's/./\0\n/g' | sort -u | wc -l) -eq $(($1 + 1)) ] && break
    done
    echo $n
}

read s
mark 4
mark 14
