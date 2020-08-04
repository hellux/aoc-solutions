#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

#define PART1_SPRINGSCRIPT "NOT C J\n" \
                           "AND D J\n" \
                           "NOT A T\n" \
                           "OR T J\n"

void execute(const char *script, struct context *cpu) {
    size_t i = 0;
    while (script[i] != '\0') {
        run_until_stop(cpu);
        while (cpu->status == STATUS_WAIT_OUT) {
            take(cpu);
            run_until_stop(cpu);
        }

        do {
            give(cpu, (integer) script[i++]);
        } while (script[i-1] != '\n');
    }

    give_string(cpu, "WALK\n");
}

int part1(struct context cpu) {
    execute(PART1_SPRINGSCRIPT, &cpu);

    integer output = 0;
    while (output < 0xff) {
        output = take(&cpu);
    }
    
    return (int) output;
}

int main(void) {
    struct context ini;
    init_context(&ini);

    printf("%d\n", part1(ini));

    return EXIT_SUCCESS;
}
