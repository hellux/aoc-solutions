#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define BITMASK_WIDTH 36
#define MEMSIZE 100000
#define PROGRAM_SIZE 600

typedef uint8_t op_t;
typedef uint64_t val_t;
typedef uint64_t addr_t;
typedef uint64_t bitmask_t; // ternary number, digits 0|1|X

#define OP_HALT 0
#define OP_SET_MASK 1
#define OP_WRITE 2

typedef struct {
    op_t op;
    union {
        struct {
            uint32_t address;
            uint32_t value;
        };
        bitmask_t bitmask;
    };
} instr_t;

typedef struct {
    addr_t addr;
    val_t val;
} slot_t;

typedef struct {
    int count;
    slot_t slots[MEMSIZE];
} memory_t;

typedef instr_t *program_t;

typedef struct {
    uint32_t pc;
    bitmask_t bitmask;
    memory_t *memory;
} context_t;

#define BIT_0 0
#define BIT_1 1
#define BIT_X 2

void write(memory_t *mem, addr_t a, val_t v) {
    for (int i = 0; i < mem->count; i++) {
        if (mem->slots[i].addr == a) {
            mem->slots[i].val = v;
            return;
        }
    }

    if (mem->count >= MEMSIZE)
        exit(4);

    mem->slots[mem->count].addr = a;
    mem->slots[mem->count].val = v;
    mem->count++;
}

val_t exp3(int exp) {
    val_t base = 3;
    val_t ret = 1;
    while (1) {
        if (exp & 1) ret *= base;
        exp >>= 1;
        if (!exp) break;
        base *= base;
    }

    return ret;
}

val_t mask_val(bitmask_t bm, val_t v) {
    val_t ret = 0;
    for (int i = 0; i < BITMASK_WIDTH; i++) {
        val_t t = (bm / exp3(i)) % 3;
        if (t == BIT_X) t = (v >> i) & 1;
        ret += t << i;
    }
    return ret;
}

int num_x(bitmask_t bm) {
    int count = 0;
    for (int i = 0; i < BITMASK_WIDTH; i++) {
        val_t t = (bm / exp3(i)) % 3;
        if (t == BIT_X)
            count++;
    }
    return count;
}

addr_t mask_addr(bitmask_t bm, addr_t a, val_t f) {
    val_t ret = 0;
    int i_floating = 0;
    for (int i = 0; i < BITMASK_WIDTH; i++) {
        val_t x = 0;
        switch ((bm / exp3(i)) % 3) {
            case BIT_0: x = (a >> i) & 1; break;
            case BIT_1: x = 1; break;
            case BIT_X: x = (f >> i_floating++) & 1; break;
        }
        ret += x << i;
    }
    return ret;
}

bitmask_t parse_bitmask(char *str) {
    val_t bm = 0;
    for (int i = 0; i < BITMASK_WIDTH; i++) {
        int pos = BITMASK_WIDTH-i-1;
        val_t trit;
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
        uint32_t addr, val;
        char bm[BITMASK_WIDTH];
        if (sscanf(line, "mask = %s\n", bm)) {
            instr.op = OP_SET_MASK;
            instr.bitmask = parse_bitmask(bm);
        } else if (sscanf(line, "mem[%u] = %u\n", &addr, &val)) {
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

val_t context_execute(const program_t prgm, int part) {
    context_t ctx;
    ctx.pc = 0;
    ctx.bitmask = 0;
    ctx.memory = malloc(sizeof(memory_t));

    while (1) {
        instr_t instr = prgm[ctx.pc++];
        switch (instr.op) {
            case OP_HALT:
                goto halt;
            case OP_SET_MASK:
                ctx.bitmask = instr.bitmask;
                break;
            case OP_WRITE:
                if (part == 1) {
                    val_t masked = mask_val(ctx.bitmask, instr.value);
                    write(ctx.memory, instr.address, masked);
                } else {
                    val_t floating = (val_t)1 << num_x(ctx.bitmask);
                    for (val_t f = 0; f < floating; f++) {
                        addr_t addr = mask_addr(ctx.bitmask, instr.address, f);
                        write(ctx.memory, addr, instr.value);
                    }
                }
        }
    }

halt:
    val_t sum = 0;
    for (int i = 0; i < ctx.memory->count; i++) {
        sum += ctx.memory->slots[i].val;
    }

    free(ctx.memory);

    return sum;
}

int main() {
    program_t prgm = parse_program();
    printf("%lu\n", context_execute(prgm, 1));
    printf("%lu\n", context_execute(prgm, 2));
    free(prgm);
}
