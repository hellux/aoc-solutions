#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TREE '#'

struct map {
    int width, height;
    char *squares;
};

struct map read_map() {
    fseek(stdin, 0, SEEK_END);
    size_t len = (size_t) ftell(stdin);
    fseek(stdin, 0, SEEK_SET);
    char *squares = malloc(len);
    fread(squares, 1, len, stdin);

    struct map m;
    m.width = (int) (strchr(squares, '\n') - squares);
    m.height = (int) len / (m.width+1);
    m.squares = squares;

    return m;
}

char get_square(struct map *m, int x, int y) {
    int i = y * (m->width + 1) + (x % m->width);
    return m->squares[i];
}

long check_slope(struct map *m, int dx, int dy) {
    long trees = 0;

    int x = 0;
    int y = 0;
    while (y < m->height) {
        x += dx;
        y += dy;
        trees += get_square(m, x, y) == TREE ? 1 : 0;
    }

    return trees;
}

long part1(struct map *m) {
    return check_slope(m, 3, 1);
}

long part2(struct map *m) {
    return check_slope(m, 1, 1)
         * check_slope(m, 3, 1)
         * check_slope(m, 5, 1)
         * check_slope(m, 7, 1)
         * check_slope(m, 1, 2);
}

int main() {
    struct map m = read_map();

    printf("%ld\n", part1(&m));
    printf("%ld\n", part2(&m));

    free(m.squares);

    return EXIT_SUCCESS;
}
