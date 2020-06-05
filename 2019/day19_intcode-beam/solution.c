#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "../intcode.h"

#define UNKNOWN 0
#define PULLING 1
#define STATIONARY 2

#define MAX_SIZE 2000
#define P1_SIZE 50
#define P2_SIZE 100

typedef uint8_t map[MAX_SIZE][MAX_SIZE];

void draw_map(int xstart, int ystart, int xend, int yend,
              int xm, int ym, map m) {
    for (int y = ystart; y <= yend; y++) {
        for (int x = xstart; x <= xend; x++) {
            char c = ' ';
            if (x == xm && y == ym) {
                c = '*';
            } else {
                uint8_t s = m[y][x];
                switch (s) {
                case UNKNOWN: c = ' '; break;
                case PULLING: c = '#'; break;
                case STATIONARY: c = '.'; break;
                }
            }
            printf("%c", c);
        }
        printf("\n");
    }
}

int status(int x, int y, map m, struct context cpu) {
    if (m[y][x] == UNKNOWN) {
        give(&cpu, (integer) x);
        give(&cpu, (integer) y);
        integer pulling = take(&cpu);

        m[y][x] = pulling ? PULLING : STATIONARY;
    }

    return m[y][x];
}

int part1(struct context *initial, map m) {
    int amount_affected = 0;

    for (int y = 0; y < P1_SIZE; y++) {
        for (int x = 0; x < P1_SIZE; x++) {
            if (status(x, y, m, *initial) == PULLING)
                amount_affected++;
        }
    }

    return amount_affected;
}

int part2(struct context *initial, map m) {
    int x = 0, y = 0;
    for (int i = 0; i < P1_SIZE; i++) {
        if (status(P1_SIZE-1, i, m, *initial) == PULLING) {
            x = P1_SIZE-1;
            y = i;
            break;
        } else if (status(i, P1_SIZE-1, m, *initial) == PULLING) {
            x = i;
            y = P1_SIZE-1;
            break;
        }
    }

    int i = 0;
    while (i++ < 10000000) {
        int xdr = 0, ydr = 0;

        while (status(x+1+xdr++, y, m, *initial) == PULLING);
        while (status(x, y+1+ydr++, m, *initial) == PULLING);

        if (xdr >= P2_SIZE && ydr >= P2_SIZE)
            break;
        
        if (xdr < P2_SIZE) {
            y++;
            while (status(x++, y, m, *initial) == STATIONARY);
        } else {
            x++;
        }
    }

    return 10000*x + y;
}

int main() {
    struct context cpu;
    init_context(&cpu);
    map m = {UNKNOWN};

    printf("%d\n", part1(&cpu, m));
    printf("%d\n", part2(&cpu, m));

    return EXIT_SUCCESS;
}
