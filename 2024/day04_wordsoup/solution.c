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

bool cross_mas(char (*grid)[MAX_SIZE][MAX_SIZE], const size_t x, const size_t y) {
    static const char *MAS[] = {"MAS", "SAM"};
    static size_t L = 3;

    for (size_t d = 0; d < 2; d++) {
        bool line_match = false;
        for (size_t i = 0; i < sizeof(MAS) / sizeof(MAS[0]); i++) {
            bool dir_match = true;
            for (size_t j = 0; j < L; j++) {
                const size_t xx = d == 0 ? y + j : y + L - j - 1;
                const size_t yy = x + j;
                if ((*grid)[yy][xx] != MAS[i][j]) {
                    dir_match = false;
                    break;
                }
            }
            if (dir_match) {
                line_match = true;
                break;
            }
        }
        if (!line_match)
            return false;
    }

    return true;
}

size_t part2(char (*grid)[MAX_SIZE][MAX_SIZE], const size_t w, const size_t h) {
    size_t n = 0;

    for (size_t y = 0; y < h; y++) {
        for (size_t x = 0; x < w; x++) {
            n += cross_mas(grid, x, y);
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
    for (size_t x = 0; x < w; x++)
        grid[h][x] = '\0';

    printf("%lu\n", part1(&grid, w, h));
    printf("%lu\n", part2(&grid, w, h));
}
