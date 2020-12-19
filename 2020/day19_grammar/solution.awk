function create_rule_regex(i, _rule_parts, _full_rule, _r) {
    split(rules[i], _rule_parts)

    _full_rule = ""
    for (_r = 1; _r <= length(_rule_parts); _r++) {
        rule = _rule_parts[_r]
        if (rule ~ /[0-9]+/) {
            _full_rule = _full_rule "(" create_rule_regex(rule) ")"
        } else {
            _full_rule = _full_rule rule
        }
    }

    return _full_rule
}

$1 ~ /[0-9]+:/ {
    sub(":", "", $0)
    gsub("\"", "", $0)
    num = $1; $1 = ""
    rules[num] = $0
}
$0 == "" {
    rule0p1 = "^" create_rule_regex(0) "$";
    rules[8] = "42 +"
    rules[11] = "42 31"
    for (i = 2; i < 10; i++) {
        rules[11] = rules[11] " |"
        for (j = 0; j < i; j++) {
            rules[11] = rules[11] " 42"
        }
        for (j = 0; j < i; j++) {
            rules[11] = rules[11] " 31"
        }
    }
    rule0p2 = "^" create_rule_regex(0) "$";
}
$0 ~ /[ab]+/ {
    if ($1 ~ rule0p1) { part1 += 1 }
    if ($1 ~ rule0p2) { part2 += 1 }
}
END { print part1 "\n" part2 }
