#include <stdlib.h>
#include <stdio.h>

#define MAX_RAM 1000

int mem[MAX_RAM];
int n = 0;
int pc = 0;

void add() {
    int a = mem[pc++];
    int b = mem[pc++];
    int c = mem[pc++];
    mem[c] = mem[a] + mem[b];
}

void multiply() {
    int a = mem[pc++];
    int b = mem[pc++];
    int c = mem[pc++];
    mem[c] = mem[a] * mem[b];
}

int main(void) {
    /* load program to mem */
    while (scanf("%d,", mem+n) == 1) {
        n++;
    }

    /* restore error state */
    mem[1] = 12;
    mem[2] = 2;

    /* execute program */
    int quit = 0;
    while (!quit && pc < n) {
        int instr = mem[pc++];
        switch (instr) {
        case 1: add(); break;
        case 2: multiply(); break;
        case 99: quit=1; break;
        }
    }

    printf("part1: %d\n", mem[0]);

    return EXIT_SUCCESS;
}
