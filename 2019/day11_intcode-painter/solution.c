#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

#define HULL_WIDTH 110
#define HULL_HEIGHT 100

#define BLACK 0
#define WHITE 1
#define UNPAINTED -1

#define LEFT 0
#define RIGHT 1

#define FOR_EACH_PANEL(xstart, ystart, xend, yend) \
    for (int y = ystart; y <= yend; y++) \
        for (int x = xstart; x <= xend; x++)

struct robot {
    struct context cpu;
    int x, y;
    int dx, dy;
};

void init_robot(struct robot *robot) {
    init_context(&robot->cpu);

    /* start in middle */
    robot->x = HULL_WIDTH / 2;
    robot->y = HULL_HEIGHT / 2;

    /* facing up */
    robot->dx = 0;
    robot->dy = -1;
}

void init_hull(int hull[][HULL_WIDTH]) {
    /* mark whole hull as unpainted */
    FOR_EACH_PANEL(0, 0, HULL_WIDTH-1, HULL_HEIGHT-1)
        hull[y][x] = UNPAINTED;
}

int get_color(int x, int y, int hull[][HULL_WIDTH]) {
    int color = hull[y][x];
    switch (color) {
    case WHITE: return color;
    case BLACK: return color;
    case UNPAINTED: return BLACK;
    default:
        fprintf(stderr, "warning: invalid color %d\n", color);
        exit(1);
    }
}

void paint(int x, int y, int color, int hull[][HULL_WIDTH]) {
    hull[y][x] = color;
}

void turn(struct robot *robot, int direction) {
    int dx = robot->dx;
    int dy = robot->dy;
    switch (direction) {
    case RIGHT:
        robot->dx = -dy;
        robot->dy =  dx;
        break;
    case LEFT:
        robot->dx =  dy;
        robot->dy = -dx;
        break;
    default:
        fprintf(stderr, "warning: invalid direction %d\n", direction);
    }
}

void move_forward(struct robot *robot) {
    robot->x += robot->dx;
    robot->y += robot->dy;
}

void run_robot(int hull[][HULL_WIDTH], struct robot *robot) {
    while (run_until_stop(&robot->cpu)) {
        int current_color = get_color(robot->x, robot->y, hull);

        give(&robot->cpu, (integer) current_color);
        int new_color = (int) take(&robot->cpu);
        int direction = (int) take(&robot->cpu);

        paint(robot->x, robot->y, new_color, hull);
        turn(robot, direction);
        move_forward(robot);
    }
}

void show_hull(int hull[][HULL_WIDTH]) {
    /* find bounding box of painted panels */
    int xmin = HULL_WIDTH;  int xmax = 0;
    int ymin = HULL_HEIGHT; int ymax = 0;
    FOR_EACH_PANEL(0, 0, HULL_WIDTH-1, HULL_HEIGHT-1) {
        if (get_color(x, y, hull) == WHITE) {
            if (x < xmin) xmin = x;
            if (x > xmax) xmax = x;
            if (y < ymin) ymin = y;
            if (y > ymax) ymax = y;
        }
    }

    FOR_EACH_PANEL(xmin, ymin, xmax, ymax) {
        printf("%s", get_color(x, y, hull) == 1 ? "█" : " ");
        if (x == xmax)
            printf("\n");
    }
}

int part1(struct robot robot) {
    int hull[HULL_HEIGHT][HULL_WIDTH];
    init_hull(hull);
    run_robot(hull, &robot);

    int painted_panels = 0;
    FOR_EACH_PANEL(0, 0, HULL_WIDTH-1, HULL_HEIGHT-1) {
        if (hull[y][x] != UNPAINTED)
            painted_panels++;
    }
    return painted_panels;
}

void part2(struct robot robot) {
    int hull[HULL_HEIGHT][HULL_WIDTH];
    init_hull(hull);

    paint(robot.x, robot.y, WHITE, hull);
    run_robot(hull, &robot);

    show_hull(hull);
}

int main (void) {
    struct robot ini;
    init_robot(&ini);

    printf("%d\n", part1(ini));
    part2(ini);

    return EXIT_SUCCESS;
}
