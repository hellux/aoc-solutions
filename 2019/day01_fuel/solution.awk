function fuel_eq(mass) { return int(mass/3)-2 }
function req_fuel(mass) {
    req=fuel_eq(mass);
    if (req > 0)
        return req+req_fuel(req)
    else
        return 0
}

{ fuel=fuel_eq($0); part1+=fuel; part2+=fuel+req_fuel(fuel) }
END { print part1,part2 }
