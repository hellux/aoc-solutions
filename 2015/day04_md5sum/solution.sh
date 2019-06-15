find_sum () {
    msg=$1; zeroes=$2

    cmp=$(printf "%0*d" $zeroes 0)

    found="false"; i=0
    while [ "$found" != "true" ]; do
        pids=""

        for i in $(seq $i $((i+PARA-1))); do
            [ "$(printf "$msg$i" | md5sum | cut -c1-$zeroes )" = "$cmp" ] \
                && echo $i &
            pids="$pids $!"
        done
        i=$((i+1))

        for pid in $pids; do
            if wait $pid; then
                found="true"
                break
            fi
        done
    done
}

PARA=2048

read input
printf "part1: %d\n" $(find_sum $input 5)
printf "part2: %d\n" $(find_sum $input 6)
