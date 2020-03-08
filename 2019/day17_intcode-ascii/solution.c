#include <stdlib.h>
#include <stdio.h>

#define MAX_CAMERA_OUTPUT 2000
#define MAX_INSTR 1000

#define SCAFFOLD    '#'
#define SPACE       '.'
#define ROBOT_UP    '^'
#define ROBOT_DOWN  'v'
#define ROBOT_LEFT  '<'
#define ROBOT_RIGHT '>'
#define ROBOT_DEAD  'X'

#define RIGHT   'R'
#define LEFT    'L'

#define EAST    0
#define NORTH   1
#define WEST    2
#define SOUTH   3

#define W 29
#define H 41

#define MAX_RAM 5000
#include "../intcode.h"

typedef char map[H][W];

int is_robot(char c) {
    switch (c) {
    case ROBOT_UP:
    case ROBOT_DOWN:
    case ROBOT_LEFT:
    case ROBOT_RIGHT:
        return 1;
    default:
        return 0;
    }
}

int is_scaffold(char c) { return c == SCAFFOLD || is_robot(c); }
int inside_map(int x, int y) { return 0 <= x && x < W && 0 <= y && y < H; }

int robot_orientation(char r) {
    switch (r) {
    case ROBOT_RIGHT:   return EAST;
    case ROBOT_UP:      return NORTH;
    case ROBOT_LEFT:    return WEST;
    case ROBOT_DOWN:    return SOUTH;
    default:            return -1;
    }
}

int turn(char dir, int orientation) {
    switch (dir) {
    case LEFT:  return (orientation+1)%4;
    case RIGHT: return (orientation+3)%4;
    default:    return -1;
    }
}

int dx(int orientation) {
    switch (orientation) {
    case EAST:  return  1;
    case WEST:  return -1;
    default:    return  0;
    }
}

int dy(int orientation) {
    switch (orientation) {
    case NORTH: return -1;
    case SOUTH: return  1;
    default:    return  0;
    }
}

void get_map(struct context cpu, map m, int *rx, int *ry) {
    int x = 0;
    int y = 0;
    run_until_stop(&cpu);
    while (cpu.status == STATUS_WAIT_OUT) {
        char pixel = (char) take(&cpu);
        if (pixel == '\n') {
            x = 0; y++;
        } else {
            if (is_robot(pixel)) {
                if (rx) *rx = x;
                if (ry) *ry = y;
            }
            m[y][x++] = pixel;
        }
        run_until_stop(&cpu);
    }
}

void show_map(map m) {
    for (int y = 0; y < H; y++) {
        for (int x = 0; x < W; x++) {
            printf("%c", m[y][x]);
        }
        printf("\n");
    }
}

int part1(map m) {
    int sum = 0;
    for (int y = 1; y < H-1; y++) {
        for (int x = 1; x < W-1; x++) {
            if (is_scaffold(m[y][x])) {
                int is_intersection = 1;
                for (int ori = EAST; ori < SOUTH; ori++) {
                    int adj_x = x + dx(ori);
                    int adj_y = y + dy(ori);
                    if (m[adj_y][adj_x] != SCAFFOLD)
                        is_intersection = 0;
                }
                if (is_intersection)
                    sum += x*y;
            }
        }
    }

    return sum;
}

int possible_steps(int orientation, int x, int y, map m) {
    int steps = 0;
    do {
        x += dx(orientation);
        y += dy(orientation);
        steps++;
    } while (inside_map(x, y) && is_scaffold(m[y][x]));
    return steps-1;
}

void get_path(map m, int x, int y,
              char dirs[], int steps[], int *n) {
    *n = 0;
    int ori = robot_orientation(m[y][x]);
    while (1) {
        int possible_left = possible_steps(turn(LEFT, ori), x, y, m);
        int possible_right = possible_steps(turn(RIGHT, ori), x, y, m);

        char dir;
        int step = 0;
        if (possible_left > 0) {
            dir = LEFT;
            step = possible_left;
        } else if (possible_right > 0) {
            dir = RIGHT;
            step = possible_right;
        } else {
            break;
        }

        ori = turn(dir, ori);
        x += step * dx(ori);
        y += step * dy(ori);
        dirs[*n] = dir;
        steps[*n] = step;
        (*n)++;
    }

}

integer read(struct context *cpu) {
    integer out;
    do {
        out = take(cpu);
    } while (cpu->status == STATUS_WAIT_OUT);
    return out;
}

void give_string(const char *str, struct context *cpu) {
    for (int i = 0; str[i] != 0; i++)
        give(cpu, (integer) str[i]);
    give(cpu, (integer) '\n');
}

integer execute_routines(const char *main,
                         const char *a,
                         const char *b,
                         const char *c,
                         struct context cpu) {
    cpu.mem[0] = 2;

    read(&cpu);
    give_string(main, &cpu);

    read(&cpu);
    give_string(a, &cpu);
    read(&cpu);
    give_string(b, &cpu);
    read(&cpu);
    give_string(c, &cpu);

    read(&cpu);
    give_string("n", &cpu);

    integer dust = read(&cpu); 

    return dust;
}

integer part2(struct context cpu, map m, int x, int y) {
    char dirs[MAX_INSTR];
    int steps[MAX_INSTR];
    int n;
    get_path(m, x, y, dirs, steps, &n);

    /* TODO calculate subpaths automatically */
    char *main = "A,B,A,C,A,B,C,B,C,B";
    char *a = "R,10,R,10,R,6,R,4";
    char *b = "R,10,R,10,L,4";
    char *c = "R,4,L,4,L,10,L,10";

    integer dust = execute_routines(main, a, b, c, cpu);

    return dust;
}

int main() {
    struct context cpu;
    init_context(&cpu);

    map m;
    int rx, ry;
    get_map(cpu, m, &rx, &ry);

    printf("%d\n", part1(m));
    printf("%ld\n", part2(cpu, m, rx, ry));

    return EXIT_SUCCESS;
}
