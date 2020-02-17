#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

#define SCREEN_WIDTH 50
#define SCREEN_HEIGHT 50

#define EMPTY 0
#define WALL 1
#define BLOCK 2
#define PADDLE 3
#define BALL 4

#define FOR_EACH_TILE(xstart, ystart, xend, yend) \
    for (int y = ystart; y <= yend; y++) \
        for (int x = xstart; x <= xend; x++)

void show_screen(int screen[][SCREEN_WIDTH]) {
    /* find bounding box of painted panels */
    int xmin = SCREEN_WIDTH;  int xmax = 0;
    int ymin = SCREEN_HEIGHT; int ymax = 0;
    FOR_EACH_TILE(0, 0, SCREEN_WIDTH-1, SCREEN_HEIGHT-1) {
        if (screen[y][x] != EMPTY) {
            if (x < xmin) xmin = x;
            if (x > xmax) xmax = x;
            if (y < ymin) ymin = y;
            if (y > ymax) ymax = y;
        }
    }

    FOR_EACH_TILE(xmin, ymin, xmax, ymax) {
        char *tile;
        switch (screen[y][x]) {
        case EMPTY:     tile = " "; break;
        case WALL:      tile = "█"; break;
        case BLOCK:     tile = "#"; break;
        case PADDLE:    tile = "="; break;
        case BALL:      tile = "O"; break;
        default: exit(1);
        }
        printf("%s", tile);
        if (x == xmax)
            printf("\n");
    }
}

int part1(struct context cpu) {
    int screen[SCREEN_HEIGHT][SCREEN_WIDTH] = {0}; 

    while (run_until_stop(&cpu)) {
        int x = (int) take(&cpu);
        int y = (int) take(&cpu);
        int tile = (int) take(&cpu);

        screen[y][x] = tile;
    }

    int blocks = 0;
    FOR_EACH_TILE(0, 0, SCREEN_WIDTH-1, SCREEN_HEIGHT-1) {
        if (screen[y][x] == BLOCK)
            blocks++;
    }
    show_screen(screen);

    return blocks;
}

int main (void) {
    struct context ini;
    init_context(&ini);

    printf("%d\n", part1(ini));

    return EXIT_SUCCESS;
}
