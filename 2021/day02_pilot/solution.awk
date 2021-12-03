$1 == "forward" { x += $2; y += aim * $2 }
$1 == "up" { aim -= $2 }
$1 == "down" { aim += $2 }
END { print x * aim; print x * y }
