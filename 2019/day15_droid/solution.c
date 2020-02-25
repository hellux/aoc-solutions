#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#define MAX_RAM 4000
#include "../intcode.h"

#define WORLD_SIZE 100
#define START_X WORLD_SIZE/2
#define START_Y WORLD_SIZE/2

#define NORTH 1
#define SOUTH 2
#define WEST 3
#define EAST 4

#define WALL 0
#define EMPTY 1
#define OXYGEN 2
#define UNKNOWN 3

#define FOR_EACH_TILE(xstart, ystart, xend, yend) \
    for (int y = ystart; y <= yend; y++) \
        for (int x = xstart; x <= xend; x++)

typedef int world[WORLD_SIZE][WORLD_SIZE];

struct droid {
    struct context cpu;
    int x;
    int y;
};

void init_droid(struct droid *d) {
    init_context(&d->cpu);
    d->x = START_X;
    d->y = START_Y;
}

void init_world(world w, struct droid *d) {
    FOR_EACH_TILE(0, 0, WORLD_SIZE-1, WORLD_SIZE-1) {
        w[y][x] = UNKNOWN;
    }
    w[d->y][d->x] = EMPTY;
}

int dx(int dir) {
    switch (dir) {
    case NORTH:
    case SOUTH: return 0;
    case EAST: return 1;
    case WEST: return -1;
    }
    return 0;
}

int dy(int dir) {
    switch (dir) {
    case NORTH: return -1;
    case SOUTH: return 1;
    case EAST:
    case WEST: return 0;
    }
    return 0;
}

void bounds(world w, int *xmin, int *ymin, int *xmax, int *ymax) {
    *xmin = WORLD_SIZE; *xmax = 0;
    *ymin = WORLD_SIZE; *ymax = 0;
    FOR_EACH_TILE(0, 0, WORLD_SIZE-1, WORLD_SIZE-1) {
        if (w[y][x] == EMPTY || w[y][x] == OXYGEN) {
            if (x < *xmin) *xmin = x;
            if (x > *xmax) *xmax = x;
            if (y < *ymin) *ymin = y;
            if (y > *ymax) *ymax = y;
        }
    }
    (*xmin)--; (*xmax)++;
    (*ymin)--; (*ymax)++;
}

void print_world(world w, struct droid *d) {
    int xmin, ymin, xmax, ymax;
    bounds(w, &xmin, &ymin, &xmax, &ymax);
    FOR_EACH_TILE(xmin, ymin, xmax, ymax) {
        char *tile = " ";
        if (x == d->x && y == d->y) {
            tile = "¤";
        } else {
            switch (w[y][x]) {
            case EMPTY:     tile = " "; break;
            case WALL:      tile = "█"; break;
            case OXYGEN:    tile = "o"; break;
            case UNKNOWN:   tile = "."; break;
            }
        }
        printf("%s", tile);
        if (x == xmax)
            printf("\n");
    }
}

int query(int dir, struct droid *d) {
    give(&d->cpu, (long) dir);
    int status = (int) take(&d->cpu);
    if (status < 0 || status > OXYGEN) {
        fprintf(stderr, "invalid status: %d\n", status);
        exit(1);
    }
    return status;
}

int move(int dir, struct droid *d, world w) {
    int x = d->x + dx(dir);
    int y = d->y + dy(dir);

    w[y][x] = query(dir, d);

    int tile = w[y][x];

    if (tile != WALL) {
        d->x = x;
        d->y = y;
    }

    return tile;
}

#define UNSEEN 0
#define UNVISITED 1
#define VISITED 2

struct node {
    int cost;
    int visited;
    int x;
    int y;
    struct node *previous;
    int dir;
};

struct node *cheapest_unvisited(struct node *nodes, int n) {
    struct node *cheap = NULL;
    for (int i = 1; i < n; i++) {
        if (nodes[i].visited == UNVISITED) {
            if (cheap == NULL || nodes[i].cost < cheap->cost) {
                cheap = &nodes[i];
            }
        }
    }

    return cheap;
}

/* dijkstra */
int *path_to_closest(int tile, int x, int y, world w,
                     int *destx, int *desty, int *length) {
    int *path = NULL;

    int xmin, ymin, xmax, ymax;
    bounds(w, &xmin, &ymin, &xmax, &ymax);
    int width  = xmax - xmin + 1;
    int height = ymax - ymin + 1;

    int n = width*height;
    struct node *nodes = calloc(sizeof(struct node), (size_t) n);
    for (int i = 0; i < n; i++) {
        nodes[i].cost = INT_MAX;
        nodes[i].visited = UNSEEN;
        nodes[i].previous = NULL;
        nodes[i].x = xmin + i % width;
        nodes[i].y = ymin + i / width;
    }

    int i_start = (y-ymin)*width+(x-xmin);
    struct node *start = &nodes[i_start];
    start->cost = 0;
    start->visited = UNVISITED;

    /* find wanted tile */
    struct node *current;
    while (1) {
        current = cheapest_unvisited(nodes, n);

        if (!current)
            goto finish;

        int current_tile = w[current->y][current->x];
        if (current_tile == tile) {
            break;
        } else if (current_tile == EMPTY || current_tile == OXYGEN) {
            for (int dir = NORTH; dir <= EAST; dir++) {
                int xn = current->x + dx(dir);
                int yn = current->y + dy(dir);
                if (xmin <= xn && xn <= xmax && ymin <= yn && yn <= ymax) {
                    struct node *neigh = &nodes[(yn-ymin)*width+(xn-xmin)];
                    int cost = current->cost + 1;

                    if (cost < neigh->cost) {
                        neigh->previous = current;
                        neigh->cost = cost;
                        neigh->visited = UNVISITED;
                        neigh->dir = dir;
                    }
                }
            }
        }

        current->visited = VISITED;
    }

    if (destx)
        *destx = current->x;
    if (desty)
        *desty = current->y;
    if (length)
        *length = current->cost;

    /* backtrack to build path */
    path = calloc(sizeof(int), (size_t) current->cost+1);
    for (int i = current->cost-1; i >= 0; i--) {
        path[i] = current->dir;
        current = current->previous;
    }

finish:
    free(nodes);
    return path;
}

void explore(struct droid *d, world w) {
    while (1) {
        int *path = path_to_closest(UNKNOWN, d->x, d->y, w,
                                    NULL, NULL, NULL);

        if (path) {
            /* examine closest unknown tile */
            for (int i = 0; path[i]; i++) {
                move(path[i], d, w);
            }
            free(path);
        } else {
            /* no more unknown tiles within reach */
            break;
        }
    }
}

int part1(world w) {
    int length = -1;
    path_to_closest(OXYGEN, START_X, START_Y, w, NULL, NULL, &length);
    return length;
}

int part2(world w) {
    int ox, oy;
    int *path = path_to_closest(OXYGEN, START_X, START_Y, w, &ox, &oy, NULL);

    if (!path) {
        fprintf(stderr, "oxygen not found\n");
        return -1;
    }

    int max_length = -1;
    while (1) {
        int ex, ey, el;
        int *path = path_to_closest(EMPTY, ox, oy, w, &ex, &ey, &el);

        if (path) {
            w[ey][ex] = OXYGEN;
            if (el > max_length)
                max_length = el;
        } else {
            return max_length;
        }
    }
}

int main() {
    struct droid d;
    init_droid(&d);
    world w;
    init_world(w, &d);

    explore(&d, w);
    /* print_world(w, &d); */

    printf("%d\n", part1(w));
    printf("%d\n", part2(w));
}
