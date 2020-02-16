#include <stdlib.h>
#include <stdio.h>

#define OP_ADD  1
#define OP_MUL  2
#define OP_IN   3
#define OP_OUT  4
#define OP_JNZ  5
#define OP_JZ   6
#define OP_LT   7
#define OP_EQ   8
#define OP_REL  9
#define OP_HLT 99

#define POSITION_MODE 0
#define IMMEDIATE_MODE 1
#define RELATIVE_MODE 2

#define STATUS_HALT 0
#define STATUS_RUN 1
#define STATUS_WAIT_IN 2
#define STATUS_WAIT_OUT 3

#define MAX_RAM 100000
#define MAX_PARAMS 3

#define UNUSED(x) (void)(x)

typedef long integer;

struct instruction {
    int opcode;
    int n;
    int modes[MAX_PARAMS];
    integer values[MAX_PARAMS];
    integer addrs[MAX_PARAMS];
};

struct context {
    int status;

    integer mem[MAX_RAM];
    int n;
    integer pc;

    integer relative_base;

    integer input;
    int new_input;

    integer output;
    int output_taken;

    long cycles;
};

int OPCODE_PARAM_COUNTS[100] = {0};
void (*OP_FUNCS[100])(struct instruction, struct context *ctx) = {0};

void add(struct instruction instr, struct context *ctx) {
    ctx->mem[instr.addrs[2]] = instr.values[0] + instr.values[1];
}

void mul(struct instruction instr, struct context *ctx) {
    ctx->mem[instr.addrs[2]] = instr.values[0] * instr.values[1];
}

void in(struct instruction instr, struct context *ctx) {
    if (ctx->new_input) {
        ctx->mem[instr.addrs[0]] = ctx->input;
        ctx->new_input = 0;
        ctx->status = STATUS_RUN;
    } else {
        ctx->status = STATUS_WAIT_IN;
        ctx->pc -= 1 + OPCODE_PARAM_COUNTS[OP_IN];
    }
}

void out(struct instruction instr, struct context *ctx) {
    if (ctx->output_taken) {
        ctx->status = STATUS_RUN;
        ctx->output_taken = 0;
    } else {
        ctx->output = instr.values[0];
        ctx->status = STATUS_WAIT_OUT;
        ctx->pc -= 1 + OPCODE_PARAM_COUNTS[OP_OUT];
    }
}

void jnz(struct instruction instr, struct context *ctx) {
    if (instr.values[0] != 0)
        ctx->pc = instr.values[1];
}

void jz(struct instruction instr, struct context *ctx) {
    if (instr.values[0] == 0)
        ctx->pc = instr.values[1];
}

void lt(struct instruction instr, struct context *ctx) {
    ctx->mem[instr.addrs[2]] = instr.values[0] < instr.values[1];
}

void eq(struct instruction instr, struct context *ctx) {
    ctx->mem[instr.addrs[2]] = instr.values[0] == instr.values[1];
}

void rel(struct instruction instr, struct context *ctx) {
    ctx->relative_base += instr.values[0];
}

void hlt(struct instruction instr, struct context *ctx) {
    UNUSED(instr);
    ctx->status = STATUS_HALT;
}

void initialize() {
    OPCODE_PARAM_COUNTS[OP_ADD] = 3;
    OPCODE_PARAM_COUNTS[OP_MUL] = 3;
    OPCODE_PARAM_COUNTS[OP_IN]  = 1;
    OPCODE_PARAM_COUNTS[OP_OUT] = 1;
    OPCODE_PARAM_COUNTS[OP_JNZ] = 2;
    OPCODE_PARAM_COUNTS[OP_JZ]  = 2;
    OPCODE_PARAM_COUNTS[OP_LT]  = 3;
    OPCODE_PARAM_COUNTS[OP_EQ]  = 3;
    OPCODE_PARAM_COUNTS[OP_REL] = 1;
    OPCODE_PARAM_COUNTS[OP_HLT] = 0;

    OP_FUNCS[OP_ADD] = add;
    OP_FUNCS[OP_MUL] = mul;
    OP_FUNCS[OP_IN]  = in;
    OP_FUNCS[OP_OUT] = out;
    OP_FUNCS[OP_JNZ] = jnz;
    OP_FUNCS[OP_JZ]  = jz;
    OP_FUNCS[OP_LT]  = lt;
    OP_FUNCS[OP_EQ]  = eq;
    OP_FUNCS[OP_REL] = rel;
    OP_FUNCS[OP_HLT] = hlt;
}

void print_array(integer *arr, int n, int w, int hl) {
    /* print address bar */
    int width = w;
    if (n < w)
        width = n;
    for (int i = 0; i < width; i++) {
        printf("%d\t", i);
    }

    /* print contents */
    printf("\n");
    for (int i = 0; i < n; i++) {
        if (i > 0 && i % width == 0)
            printf("\n");
        if (hl >= 0 && i == hl) {
            printf("(%ld)\t", arr[i]);
        } else {
            printf("%ld\t", arr[i]);
        }
    }
    printf("\n");
}

void print_context(struct context *ctx) {
    printf("Memory:\n");
    print_array(ctx->mem, ctx->n, 10, (int) ctx->pc);
    printf("\nInput: %ld, new: %d\n", ctx->input, ctx->new_input);
    printf("\nOutput: %ld, taken: %d\n", ctx->output, ctx->output_taken);
    printf("\nstatus: %d, n: %d, pc: %ld, rb: %ld, cycles: %ld\n",
           ctx->status, ctx->n, ctx->pc, ctx->relative_base, ctx->cycles);
}

struct instruction get_instr(struct context *ctx) {
    struct instruction instr;

    /* instr code */
    int code = (int) ctx->mem[ctx->pc++];
    instr.opcode = code % 100;
    instr.n = OPCODE_PARAM_COUNTS[instr.opcode];

    /* parameters */
    int modes = code / 100;
    for (int i = 0; i < instr.n; i++) {
        int mode = modes % 10;
        integer raw = ctx->mem[ctx->pc++];
        integer value, addr;
        switch (mode) {
        case POSITION_MODE:
            value = ctx->mem[raw];
            addr = raw;
            break;
        case IMMEDIATE_MODE:
            value = raw;
            addr = ctx->pc-1;
            break;
        case RELATIVE_MODE:
            value = ctx->mem[ctx->relative_base + raw];
            addr = ctx->relative_base + raw;
            break;
        default:
            value = 0;
            addr = 0;
            fprintf(stderr, "error: invalid paramater mode -- %d", mode);
        }

        modes = modes / 10;

        instr.modes[i] = mode;
        instr.values[i] = value;
        instr.addrs[i] = addr;
    }

    return instr;
}

int tick(struct context *ctx) {
    if (ctx->pc < 0 || ctx->pc >= ctx->n)
        ctx->status = STATUS_HALT;

    if (ctx->status != STATUS_HALT) {
        struct instruction instr = get_instr(ctx);
        OP_FUNCS[instr.opcode](instr, ctx);
    }

    ctx->cycles++;

    return ctx->status;
}

int run_until_stop(struct context *ctx) {
    while (tick(ctx) == STATUS_RUN);
    return ctx->status;
}

void give(struct context *ctx, integer input) {
    run_until_stop(ctx);
    if (ctx->new_input == 1)
        printf("warning: overwriting unreceived input %ld with %ld\n",
               ctx->input, input);
    ctx->input = input;
    ctx->new_input = 1;
}

integer take(struct context *ctx) {
    run_until_stop(ctx);
    if (ctx->output_taken == 1)
        printf("warning: taking already taken output\n");
    ctx->output_taken = 1;
    return ctx->output;
}

void init_context(struct context *ctx) {
    initialize();

    ctx->status = STATUS_RUN;

    /* load program to initial memory */
    ctx->n = 0;
    while (scanf("%ld,", ctx->mem + ctx->n) == 1) {
        ctx->n++;
    }
    ctx->pc = 0;

    ctx->relative_base = 0;

    ctx->input = 0;
    ctx->new_input = 0;

    ctx->output = 0;
    ctx->output_taken = 0;

    ctx->cycles = 0;
}
