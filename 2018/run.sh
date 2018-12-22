# usage: ./run.sh day or ./run.sh -i "INPUT" day

input=""
while getopts i: flag; do
    case "$flag" in
        i) input=$OPTARG;;
    esac
done
shift $((OPTIND-1))

day=$1
day_dir="$(echo day$(printf "%02d" "$day")*)"

[ -z "$input" ] && input="$(cat "$day_dir/input")"

printf "$input" | go run "$day_dir/solution.go"
