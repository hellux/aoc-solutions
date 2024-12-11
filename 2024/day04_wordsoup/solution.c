#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 256

size_t count_instances(const char *s, const char *t) {
    const size_t l = strlen(t);
    size_t n = 0;
    while ((s = strstr(s, t))) {
        s += l;
        n++;
    }
    return n;
}

size_t count_xmas(const char *s) { return count_instances(s, "XMAS") + count_instances(s, "SAMX"); }

size_t part1(char (*grid)[MAX_SIZE][MAX_SIZE], const size_t w, const size_t h) {
    size_t n = 0;

    /* rows */
    for (size_t y = 0; y < h; y++)
        n += count_xmas((*grid)[y]);

    /* columns */
    for (size_t x = 0; x < w; x++) {
        char buf_line[MAX_SIZE];
        for (size_t y = 0; y < h; y++)
            buf_line[y] = (*grid)[y][x];
        buf_line[h] = '\0';
        n += count_xmas(buf_line);
    }

    /* diagonals */
    const size_t diag = w + h - 1;
    for (size_t i = 0; i < diag; i++) {
        for (size_t d = 0; d < 2; d++) {
            /* d=0: bottom left -> top right */
            /* d=1: bottom right -> top left */
            char buf_line[MAX_SIZE];
            size_t l = 0;
            for (size_t j = 0; j < diag; j++) {
                const size_t x = d == 0 ? j : w - j - 1;
                const size_t y = i - j;
                if (y < h && x < w) {
                    buf_line[l] = (*grid)[y][x];
                    l++;
                }
            }
            buf_line[l] = '\0';
            n += count_xmas(buf_line);
        }
    }

    return n;
}

int main() {
    char grid[MAX_SIZE][MAX_SIZE];
    size_t h = 0;
    while (fgets(grid[h], MAX_SIZE, stdin))
        h++;
    const size_t w = strlen(grid[0]) - 1;

    printf("%lu\n", part1(&grid, w, h));
}
