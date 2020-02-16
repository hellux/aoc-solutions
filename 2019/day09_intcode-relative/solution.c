#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

integer part1(struct context ctx) {
    give(&ctx, 1);
    run_until_stop(&ctx);
    return ctx.output;
}

integer part2(struct context ctx) {
    give(&ctx, 2);
    run_until_stop(&ctx);
    return ctx.output;
}

int main(void) {
    initialize();
    struct context ini;
    init_context(&ini);

    printf("%ld\n", part1(ini));
    printf("%ld\n", part2(ini));

    return EXIT_SUCCESS;
}
