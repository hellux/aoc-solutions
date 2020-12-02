{
    split($1, ran, "-")
    sub(":", "", $2); ltr = $2
    pw = $3

    filtered = pw
    gsub("[^" ltr "]", "", filtered)
    occurrences = length(filtered)
    if (ran[1] <= occurrences && occurrences <= ran[2])
        part1 += 1

    if ((substr(pw, ran[1], 1) == ltr) != (substr(pw, ran[2], 1) == ltr))
        part2 += 1
}
END { print part1 "\n" part2 }
