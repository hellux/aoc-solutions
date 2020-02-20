package main

import (
    "io/ioutil"
    "os"
    "fmt"
    "strings"
    "strconv"
)

type Chemical string
type Ingredient struct {
    chemical Chemical
    amount int
}
type Recipe struct {
    ingredients []Ingredient
    amount int
}
type CookBook map[Chemical]Recipe
type Reserve map[Chemical]int

const FUEL = Chemical("FUEL")
const ORE = Chemical("ORE")

func get_recipes() CookBook {
    recipes := make(CookBook, 0)

    bytes, _ := ioutil.ReadAll(os.Stdin)
    lines := strings.Split(string(bytes), "\n")

    for _, line := range lines {
        pair := strings.Split(line, " => ")
        ingredients := strings.Split(pair[0], ", ")
        result := strings.Split(pair[1], " ")

        amount, _ := strconv.Atoi(result[0])
        chemical := Chemical(result[1])

        r := recipes[chemical]
        r.ingredients = make([]Ingredient, len(ingredients))
        r.amount = amount

        for i, ingr := range ingredients {
            pair := strings.Split(ingr, " ")
            r.ingredients[i].amount, _ = strconv.Atoi(pair[0])
            r.ingredients[i].chemical = Chemical(pair[1])
        }

        recipes[chemical] = r
    }

    return recipes
}

func craft(amount int, chemical Chemical,
           recipes CookBook, reserve Reserve) {
    recipe, craftable := recipes[chemical]

    if craftable {
        for reserve[chemical] < amount {
            for _, ingr := range recipe.ingredients {
                craft(ingr.amount, ingr.chemical, recipes, reserve)
                reserve[ingr.chemical] -= ingr.amount
            }

            reserve[chemical] += recipe.amount
        }
    }
}

func part1(recipes CookBook) int {
    reserve := make(Reserve)
    craft(1, FUEL, recipes, reserve)
    return -reserve[ORE]
}

func linearCost(amount float64, chemical Chemical, recipes CookBook) float64 {
    if (chemical == ORE) {
        return amount;
    } else {
        recipe := recipes[chemical]
        cost := 0.0
        for _, ingr := range recipe.ingredients {
            cost += linearCost(float64(ingr.amount) / float64(recipe.amount),
                               ingr.chemical, recipes)
        }
        return cost*amount
    }
}

func part2(recipes CookBook) int {
    /* solve linear simplification instead. this will give the upper bound of
     * possible fuels, but it is might actually fewer. */
    const TOTAL = 1000000000000
    cost := linearCost(1, FUEL, recipes)
    fuelsUpperBound := float64(TOTAL / cost)

    return int(fuelsUpperBound)
}

func main() {
    recipes := get_recipes()

    fmt.Println(part1(recipes))
    fmt.Println(part2(recipes))
}
