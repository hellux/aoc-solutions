#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define UNUSED(x) (void)(x)

#define MAX_RAM 1000
#define MAX_INPUT 2
#define MAX_OUTPUT 10
#define MAX_PARAMS 3

#define POSITION_MODE 0
#define IMMEDIATE_MODE 1

#define OP_ADD 1
#define OP_MUL 2
#define OP_LD  3
#define OP_OUT 4
#define OP_JNZ 5
#define OP_JZ  6
#define OP_LT  7
#define OP_EQ  8
#define OP_HLT 99

#define AMP_COUNT 5

struct instruction {
    int opcode;
    int n;
    int modes[MAX_PARAMS];
    int values[MAX_PARAMS];
    int raws[MAX_PARAMS];
};

struct context {
    int halt;

    int mem[MAX_RAM];
    int n;
    int pc;

    int input[MAX_INPUT];
    int ip;

    int output[MAX_OUTPUT];
    int op;
};

int OPCODE_PARAM_COUNTS[100] = {0};
void (*OP_FUNCS[100])(struct instruction, struct context *ctx) = {0};

void add(struct instruction instr, struct context *ctx) {
    ctx->mem[instr.raws[2]] = instr.values[0] + instr.values[1];
}

void mul(struct instruction instr, struct context *ctx) {
    ctx->mem[instr.raws[2]] = instr.values[0] * instr.values[1];
}

void ld(struct instruction instr, struct context *ctx) {
    int input = ctx->input[ctx->ip++];
    ctx->mem[instr.raws[0]] = input;
}

void out(struct instruction instr, struct context *ctx) {
    ctx->output[ctx->op++] = instr.values[0];
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
    ctx->mem[instr.raws[2]] = instr.values[0] < instr.values[1];
}

void eq(struct instruction instr, struct context *ctx) {
    ctx->mem[instr.raws[2]] = instr.values[0] == instr.values[1];
}

void hlt(struct instruction instr, struct context *ctx) {
    UNUSED(instr);
    ctx->halt = 1;
}

void initialize() {
    OPCODE_PARAM_COUNTS[OP_ADD] = 3;
    OPCODE_PARAM_COUNTS[OP_MUL] = 3;
    OPCODE_PARAM_COUNTS[OP_LD]  = 1;
    OPCODE_PARAM_COUNTS[OP_OUT] = 1;
    OPCODE_PARAM_COUNTS[OP_JNZ] = 2;
    OPCODE_PARAM_COUNTS[OP_JZ] = 2;
    OPCODE_PARAM_COUNTS[OP_LT] = 3;
    OPCODE_PARAM_COUNTS[OP_EQ] = 3;

    OP_FUNCS[OP_ADD] = add;
    OP_FUNCS[OP_MUL] = mul;
    OP_FUNCS[OP_LD]  = ld;
    OP_FUNCS[OP_OUT] = out;
    OP_FUNCS[OP_HLT] = hlt;
    OP_FUNCS[OP_JNZ] = jnz;
    OP_FUNCS[OP_JZ] = jz;
    OP_FUNCS[OP_LT] = lt;
    OP_FUNCS[OP_EQ] = eq;
}

void print_array(int *arr, int n) {
    /* print address bar */
    int width = 10;
    if (n < 10)
        width = n;
    for (int i = 0; i < width; i++) {
        printf("%d\t", i);
    }

    /* print contents */
    printf("\n");
    for (int i = 0; i < n; i++) {
        if (i > 0 && i % 10 == 0)
            printf("\n");
        printf("%d\t", arr[i]);
    }
    printf("\n");
}

void print_context(struct context *ctx) {
    printf("Memory:\n");
    print_array(ctx->mem, ctx->n);

    printf("\nInput:\n");
    print_array(ctx->input, MAX_INPUT);

    printf("\nOutput:\n");
    print_array(ctx->output, ctx->op);

    printf("\nhalt: %d, n: %d, pc: %d, ip: %d\n",
           ctx->halt, ctx->n, ctx->pc, ctx->ip);
}

struct instruction get_instr(struct context *ctx) {
    struct instruction instr;

    /* instr code */
    int code = ctx->mem[ctx->pc++];
    instr.opcode = code % 100;
    instr.n = OPCODE_PARAM_COUNTS[instr.opcode];

    /* parameters */
    int modes = code / 100;
    for (int i = 0; i < instr.n; i++) {
        int mode = modes % 10;
        int raw = ctx->mem[ctx->pc++];
        int value;
        switch (mode) {
        case POSITION_MODE:
            value = ctx->mem[raw];
            break;   
        case IMMEDIATE_MODE:
            value = raw;
            break;
        default:
            value = 0;
            fprintf(stderr, "error: invalid paramater mode -- %d", mode);
        }

        modes = modes / 10;

        instr.modes[i] = mode;
        instr.raws[i] = raw;
        instr.values[i] = value;
    }

    return instr;
}

int execute(struct context ctx, int phase, int input) {
    ctx.input[0] = phase;
    ctx.input[1] = input;

    while (!ctx.halt && ctx.pc < ctx.n) {
        struct instruction instr = get_instr(&ctx);
        OP_FUNCS[instr.opcode](instr, &ctx);
    }

    return ctx.output[ctx.op-1];
}

void init_context(struct context *ctx) {
    ctx->halt = 0;
    ctx->n = 0;
    ctx->pc = 0;
    ctx->ip = 0;
    ctx->op = 0;

    /* load program to initial memory */
    while (scanf("%d,", ctx->mem + ctx->n) == 1) {
        ctx->n++;
    }
}

/*
int *permutations_rep(int count, int range, int *n) {
    *n = pow(count, range);
    int *seqs = malloc((*n)*count*sizeof(int));

    for (int i = 0; i < *n; i++) {
        for (int j = 0; j < count; j++) {
            int cycle = pow(range, count-j-1);
            seqs[count*i+j] = i/cycle % range;
        }
    }

    return seqs;
}
*/

int factorial(int n) {
    if (n > 1) return n*factorial(n-1);
    else return 1;
}

void swap(int *x, int *y) {
    int tmp;
    tmp = *x;
    *x = *y;
    *y = tmp;
}

void permute(int *seq, int l, int r, int len, int *seqs, int *i) {
    if (l == r) {
        int *dst = &seqs[len*((*i)++)];
        for (int j = 0; j < len; j++)
            dst[j] = seq[j];
    } else {
        for (int j = l; j <= r; j++) {
            swap(seq+l, seq+j);
            permute(seq, l+1, r, len, seqs, i);
            swap(seq+l, seq+j);
        }
   }
}

int *permutations(int *seq, int length, int *n) {
    int i = 0;
    *n = factorial(length);
    int *seqs = malloc((*n)*length*sizeof(int));

    permute(seq, 0, length-1, length, seqs, &i);

    return seqs;
}

int run_sequence(struct context *ctx, int seq[]) {
    int output = 0;
    for (int i = 0; i < AMP_COUNT; i++) {
        output = execute(*ctx, seq[i], output);
    }
    return output;
}

int find_max_signal(struct context ctx) {
    int seq[AMP_COUNT];
    for (int i = 0; i < AMP_COUNT; i++)
        seq[i] = i;
    int n;
    int *perms = permutations(seq, AMP_COUNT, &n);
    
    int max = 0;
    for (int i = 0; i < n; i++) {
        int out = run_sequence(&ctx, perms+AMP_COUNT*i);
        if (out > max) {
            max = out;
        }
    }

    return max;
}

int main(void) {
    initialize();
    struct context ini;
    init_context(&ini);

    int max_signal = find_max_signal(ini);
    printf("part1: %d\n", max_signal);

    return EXIT_SUCCESS;
}
