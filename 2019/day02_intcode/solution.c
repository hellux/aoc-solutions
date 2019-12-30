#include <stdlib.h>
#include <stdio.h>

#define MAX_RAM 150

struct context {
    int mem[MAX_RAM];
    int n;
    int pc;
};

void add(struct context *ctx) {
    int a = ctx->mem[ctx->pc++];
    int b = ctx->mem[ctx->pc++];
    int c = ctx->mem[ctx->pc++];
    ctx->mem[c] = ctx->mem[a] + ctx->mem[b];
}

void multiply(struct context *ctx) {
    int a = ctx->mem[ctx->pc++];
    int b = ctx->mem[ctx->pc++];
    int c = ctx->mem[ctx->pc++];
    ctx->mem[c] = ctx->mem[a] * ctx->mem[b];
}

int execute(struct context ctx, int noun, int verb) {
    /* set noun and verb */
    ctx.mem[1] = noun;
    ctx.mem[2] = verb;

    /* execute program */
    int quit = 0;
    while (!quit && ctx.pc < ctx.n) {
        int instr = ctx.mem[ctx.pc++];
        switch (instr) {
        case 1: add(&ctx); break;
        case 2: multiply(&ctx); break;
        case 99: quit=1; break;
        }
    }

    return ctx.mem[0];
}

void init_context(struct context *ctx) {
    ctx->n = 0;
    ctx->pc = 0;

    /* load program to initial memory */
    while (scanf("%d,", ctx->mem + ctx->n) == 1) {
        ctx->n++;
    }
}

int part1(struct context *ctx) {
    return execute(*ctx, 12, 2);
}

int part2(struct context *ctx) {
    for (int noun = 0; noun <= 99; noun++) {
        for (int verb = 0; verb <= 99; verb++) {
            if (execute(*ctx, noun, verb) == 19690720)
                return noun*100 + verb;
        }
    }
    return -1;
}

int main(void) {
    struct context ini;
    init_context(&ini);
    printf("n: %d\n", ini.n);

    printf("part1: %d\n", part1(&ini));
    printf("part2: %d\n", part2(&ini));

    return EXIT_SUCCESS;
}
