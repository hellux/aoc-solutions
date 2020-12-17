#include <stdio.h>
#include <stdlib.h>

#define MAX_INSTR 1000

struct instr {
    char dir;
    int len;
};

struct instr instr[MAX_INSTR];
int n = 0;

int DIRS[4][2] = {
    { 1, 0}, // east
    { 0,-1}, // south
    {-1, 0}, // west
    { 0, 1}, // north
};

int part1() {
    int x = 0;
    int y = 0;
    int d = 0;

    for (int i = 0; i < n; i++) {
        int l = instr[i].len;
        int dt = -1;
        switch (instr[i].dir) {
            case 'E': dt = 0; break;
            case 'S': dt = 1; break;
            case 'W': dt = 2; break;
            case 'N': dt = 3; break;
            case 'F': dt = d; break;
            case 'R': d = (d + l/90) % 4; break;
            case 'L': d = (d - l/90 + 4) % 4; break;
        }

        if (dt >= 0) {
            x += l * DIRS[dt][0];
            y += l * DIRS[dt][1];
        }
    }

    return abs(x) + abs(y);
}

int part2() {
    return 0;
}

int main() {
    char dir;
    int len;

    while (scanf("%c%d\n", &dir, &len) > 0) {
        instr[n].dir = dir;
        instr[n].len = len;
        n++;
    }

    printf("%d\n%d\n", part1(), part2());
}
