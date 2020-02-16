#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

integer run_system_id(struct context ctx, integer id) {
    give(&ctx, id);

    integer output = 0;
    while (ctx.status != STATUS_HALT) {
        output = take(&ctx);
    }
    
    return output;
}

int main(void) {
    struct context ini;
    init_context(&ini);

    printf("part1: %ld\n", run_system_id(ini, 1));
    printf("part2: %ld\n", run_system_id(ini, 5));

    return EXIT_SUCCESS;
}
