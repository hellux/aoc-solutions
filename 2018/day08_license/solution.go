package main

import (
    "io/ioutil"
    "os"
    "strings"
    "fmt"
    "strconv"
)

type node struct {
    children []node
    metadata []int
}

func create_node(numbers []int) (node, int) {
    current := 0
    childc := numbers[current]; current++
    metalen := numbers[current]; current++
    children := make([]node, childc, childc)

    for c := 0; c < childc; c++ {
        child_current := 0
        children[c], child_current = create_node(numbers[current:])
        current += child_current
    }

    metadata := numbers[current:current+metalen]; current += metalen

    return node{children, metadata}, current
}

func part1(node node) int {
    sum := 0

    for _, child := range node.children {
        sum += part1(child)
    }
    for _, n := range node.metadata {
        sum += n
    }

    return sum
}

func part2(node node) int {
    sum := 0

    if len(node.children) > 0 {
        for _, n := range node.metadata {
            if 0 < n && n <= len(node.children) {
                /* XXX value could be cached */
                sum += part2(node.children[n-1])
            }
        }
    } else {
        for _, n := range node.metadata {
            sum += n
        }
    }

    return sum
}

func main() {
    bytes, _ := ioutil.ReadAll(os.Stdin)
    str_numbers := strings.Split(string(bytes), " ")
    n := len(str_numbers)
    numbers := make([]int, n, n)
    for i := 0; i < n; i++ {
        numbers[i], _ = strconv.Atoi(str_numbers[i])
    }

    tree, _ := create_node(numbers)

    fmt.Println(part1(tree))
    fmt.Println(part2(tree))
}
