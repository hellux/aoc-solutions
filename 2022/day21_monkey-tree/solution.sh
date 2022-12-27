eqs=$(cat | sed '/./s/$/$/') # append lines with $, end command in maxima

part1="print(ev(root, infeval))$"
echo "${eqs}${part1}" | maxima --very-quiet

# remove humn, replace root op with -
eqs="$(echo "$eqs" | grep -vE "^humn:" | sed '/root:/s/[+*/]/-/')"
part2="print(rhs(solve(ev(root, infeval))[1]))$"
echo "${eqs}${part2}" | maxima --very-quiet
