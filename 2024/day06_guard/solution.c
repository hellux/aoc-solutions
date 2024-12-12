#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 256

struct grid {
    char c[MAX_SIZE][MAX_SIZE];
};

struct guard {
    size_t x;
    size_t y;
    uint8_t dir;
};

struct guard move_forward(struct guard g) {
    switch (g.dir) {
    case 0: g.y--; break; /* up */
    case 1: g.x++; break; /* right */
    case 2: g.y++; break; /* down */
    case 3: g.x--; break; /* left */
    }
    return g;
}

struct guard turn_right(struct guard g) {
    g.dir = (g.dir + 1u) % 4;
    return g;
}

void show_grid(const struct grid *grid, const size_t h) {
    for (size_t y = 0; y < h; y++)
        printf("%s", grid->c[y]);
    printf("\n");
}

size_t part1(struct grid grid, const size_t w, const size_t h, struct guard g) {
    while (g.x < w && g.y < h) {
        grid.c[g.y][g.x] = 'X';
        if (getenv("DEBUG"))
            show_grid(&grid, h);
        struct guard gn = move_forward(g);
        if (grid.c[gn.y][gn.x] == '#') {
            g = turn_right(g);
        } else {
            g = gn;
        }
    }

    size_t n = 0;
    for (size_t y = 0; y < h; y++) {
        for (size_t x = 0; x < w; x++) {
            if (grid.c[y][x] == 'X')
                n++;
        }
    }
    return n;
}

int main() {
    struct grid grid;
    size_t h = 0;
    while (fgets(grid.c[h], MAX_SIZE, stdin))
        h++;
    const size_t w = strlen(grid.c[0]) - 1;

    bool guard_found = false;
    size_t x, y;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            if (grid.c[y][x] == '^') {
                guard_found = true;
                break;
            }
        }
        if (guard_found)
            break;
    }
    assert(guard_found);
    const struct guard g = {x, y, 0u};

    printf("%lu\n", part1(grid, w, h, g));
}
