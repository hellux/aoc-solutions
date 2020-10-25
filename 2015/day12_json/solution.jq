def part1: [getpath(paths(numbers))] | add;
def redObject: type == "object" and ([.[] == "red"] | any);
def part2: walk(if redObject then 0 else . end) | part1;
part1, part2
