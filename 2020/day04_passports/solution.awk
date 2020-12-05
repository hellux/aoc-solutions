function valid_height(h) {
    num = int(substr(h, 0, length(h)-2))
    cm = substr(h, length(h)-1) == "cm"
    return h ~ /[0-9]+((cm)|(in))/ &&
        ((cm && 150 <= num && num <= 193) || (!cm && 59 <= num && num <= 76))
}

function validate_passport() {
    delete p["cid"];

    if (length(p) == 7)
        part1 += 1

    if (1920 <= p["byr"] && p["byr"] <= 2002 &&
        2010 <= p["iyr"] && p["iyr"] <= 2020 &&
        2020 <= p["eyr"] && p["eyr"] <= 2030 &&
        valid_height(p["hgt"]) &&
        p["hcl"] ~ /^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$/ &&
        p["ecl"] ~ /^(amb|blu|brn|gry|grn|hzl|oth)$/ &&
        p["pid"] ~ /^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$/)
        part2 += 1
}

NF > 0 { for (i = 1; i <= NF; ++i) { split($i, kv, ":"); p[kv[1]] = kv[2] } }
NF == 0 { validate_passport(); delete p }
END { validate_passport(); print part1 "\n" part2 }
