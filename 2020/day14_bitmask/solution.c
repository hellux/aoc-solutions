#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define BITMASK_WIDTH 36
#define MEMSIZE 100000
#define PROGRAM_SIZE 600

typedef uint8_t op_t;
typedef uint64_t value_t;
typedef uint64_t bitmask_t; // ternary number, digits 0|1|X

#define OP_HALT 0
#define OP_SET_MASK 1
#define OP_WRITE 2

typedef struct {
    op_t op;
    union {
        struct {
            int address;
            value_t value;
        };
        bitmask_t bitmask;
    };
} instr_t;

typedef value_t *memory_t;
typedef instr_t *program_t;

typedef struct {
    int pc;
    bitmask_t bitmask;
    memory_t memory;
} context_t;

#define BIT_X 2

value_t exp3(int exp) {
    value_t base = 3;
    value_t ret = 1;
    while (1) {
        if (exp & 1) ret *= base;
        exp >>= 1;
        if (!exp) break;
        base *= base;
    }

    return ret;
}

value_t mask_value(bitmask_t bm, value_t v) {
    value_t ret = 0;
    for (int i = 0; i < BITMASK_WIDTH; i++) {
        value_t t = (bm / exp3(i)) % 3;
        if (t == BIT_X) t = (v >> i) & 1;
        ret += t << i;
    }
    return ret;
}

bitmask_t parse_bitmask(char *str) {
    value_t bm = 0;
    for (int i = 0; i < BITMASK_WIDTH; i++) {
        int pos = BITMASK_WIDTH-i-1;
        value_t trit;
        switch (str[i]) {
            case '0': trit = 0; break;
            case '1': trit = 1; break;
            case 'X': trit = 2; break;
            default: exit(2);
        }
        bm += trit * exp3(pos);
    }

    return bm;
}

// read from stdin
program_t parse_program() {
    program_t program = calloc(PROGRAM_SIZE, sizeof(instr_t));
    char line[100];
    int i = 0;
    while (fgets(line, 100, stdin) != NULL) {
        instr_t instr;
        int addr;
        value_t val;
        char bm[36];
        if (sscanf(line, "mask = %s\n", bm)) {
            instr.op = OP_SET_MASK;
            instr.bitmask = parse_bitmask(bm);
        } else if (sscanf(line, "mem[%d] = %lu\n", &addr, &val)) {
            instr.op = OP_WRITE;
            instr.value = val;
            instr.address = addr;
        } else {
            exit(3);
        }

        program[i++] = instr;
    }
    return program;
}

void context_execute(context_t *ctx, const program_t prgm) {
    while (1) {
        instr_t instr = prgm[ctx->pc++];
        switch (instr.op) {
            case OP_HALT:
                return;
            case OP_SET_MASK:
                ctx->bitmask = instr.bitmask;
                break;
            case OP_WRITE:
                value_t masked = mask_value(ctx->bitmask, instr.value);
                ctx->memory[instr.address] = masked;
                break;
        }
    }
}

value_t memory_sum(memory_t mem) {
    value_t ret = 0;
    for (int i = 0; i < MEMSIZE; i++) {
        ret += mem[i];
    }
    return ret;
}

int main() {
    program_t prgm = parse_program();

    context_t ctx;
    ctx.pc = 0;
    ctx.bitmask = 0;
    ctx.memory = calloc(MEMSIZE, sizeof(value_t));

    context_execute(&ctx, prgm);
    value_t sum = memory_sum(ctx.memory);
    printf("%lu\n", sum);

    free(prgm);
    free(ctx.memory);
}
