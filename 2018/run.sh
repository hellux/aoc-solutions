# usage: ./run.sh day or ./run.sh -i "INPUT" day

input=""
input_file=""
while getopts i:I: flag; do
    case "$flag" in
        i) input=$OPTARG;;
        I) input_file=$OPTARG;;
    esac
done
shift $((OPTIND-1))

day=$1
day_dir="$(echo day$(printf "%02d" "$day")*)"

if [ -z "$input" ]; then
    [ -z "$input_file" ] && input_file="$day_dir/input"
    input="$(cat "$input_file")"
fi

printf "$input" | go run "$day_dir/solution.go"
