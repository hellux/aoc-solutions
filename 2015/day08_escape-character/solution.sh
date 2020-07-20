while read -r string; do
    encoded=\"$(printf "%s" "$string" | sed 's/\\/\\\\/g;s/"/\\"/g')\"

    len_literal=$(printf "%s" "$string" | wc -c) 
    len_chars=$(($(printf "$string" | wc -c)-2))
    len_encoded=$(printf "%s" "$encoded" | wc -c)

    part1=$((part1 + len_literal-len_chars))
    part2=$((part2 + len_encoded-len_literal))
done

echo "$part1"
echo "$part2"
