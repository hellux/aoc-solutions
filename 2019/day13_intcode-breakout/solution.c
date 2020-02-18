#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

#define EMPTY 0
#define WALL 1
#define BLOCK 2
#define PADDLE 3
#define BALL 4

#define NEUTRAL 0
#define LEFT -1
#define RIGHT 1

int part1(struct context cpu) {
    int blocks = 0;

    while (run_until_stop(&cpu)) {
        take(&cpu); /* read x */
        take(&cpu); /* read y */
        int tile = (int) take(&cpu);

        if (tile == BLOCK)
            blocks++;
    }

    return blocks;
}

int part2(struct context cpu) {
    /* insert coin */
    cpu.mem[0] = 2;

    int paddle_x = 0;
    int ball_x = 0;
    int score = 0;
    while (run_until_stop(&cpu)) {
        if (cpu.status == STATUS_WAIT_IN) {
            int joystick = NEUTRAL;

            if (ball_x < paddle_x) {
                joystick = LEFT;
            } else if (paddle_x < ball_x) {
                joystick = RIGHT;
            }

            give(&cpu, joystick);
        }

        int x = (int) take(&cpu);
        take(&cpu); /* read y */
        int tile = (int) take(&cpu);

        if (x == -1) score = tile;
        else if (tile == BALL)   ball_x   = x;
        else if (tile == PADDLE) paddle_x = x;
    }

    return score;
}

int main (void) {
    struct context ini;
    init_context(&ini);

    printf("%d\n", part1(ini));
    printf("%d\n", part2(ini));

    return EXIT_SUCCESS;
}
