#include <stdio.h>
#include <stdlib.h>

#include "intcode.h"

void prompt(struct context *ctx) {
    while (ctx->status != STATUS_HALT) {
        run_until_stop(ctx);

        char output[100];
        int i = 0;
        while (ctx->status == STATUS_WAIT_OUT) {
            integer out = take(ctx);
            if (out < 256) {
                output[i++] = (char) out;
            } else {
                printf("Number output: %lu\n", out);
            }
        }
        output[i] = '\0';
        printf("%s", output);

        if (ctx->status == STATUS_HALT)
            break;

        int c = 0;
        while (c != '\n' && c != EOF) {
            c = getchar();
            give(ctx, (integer) c);
        }
    }
}

int main(int argc, char **args) {
    if (argc == 2) {
        FILE *f = fopen(args[1], "r");

        struct context cpu;
        init_context_f(&cpu, f);

        prompt(&cpu);

        return EXIT_SUCCESS;
    } else {
        printf("Provide single file with intcode program.\n");
        return EXIT_FAILURE;
    }
}
