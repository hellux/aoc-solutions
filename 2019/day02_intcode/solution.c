#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

integer try_verb_noun(struct context ctx, integer noun, integer verb) {
    /* set noun and verb */
    ctx.mem[1] = noun;
    ctx.mem[2] = verb;

    run_until_stop(&ctx);

    return ctx.mem[0];
}

integer part1(struct context *ctx) {
    return try_verb_noun(*ctx, 12, 2);
}

integer part2(struct context *ctx) {
    for (integer noun = 0; noun <= 99; noun++) {
        for (integer verb = 0; verb <= 99; verb++) {
            if (try_verb_noun(*ctx, noun, verb) == 19690720)
                return noun*100 + verb;
        }
    }
    return -1;
}

int main(void) {
    struct context ini;
    init_context(&ini);

    printf("part1: %ld\n", part1(&ini));
    printf("part2: %ld\n", part2(&ini));

    return EXIT_SUCCESS;
}
