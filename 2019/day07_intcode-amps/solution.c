#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define UNUSED(x) (void)(x)

#define MAX_RAM 1000
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

#define STATUS_HALT 0
#define STATUS_RUN 1
#define STATUS_WAIT 2

#define AMP_COUNT 5

struct instruction {
    int opcode;
    int n;
    int modes[MAX_PARAMS];
    int raws[MAX_PARAMS];
    int values[MAX_PARAMS];
};

struct context {
    int status;

    int mem[MAX_RAM];
    int n;
    int pc;

    int input;
    int new_input;

    int output;
    int new_output;
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
    if (ctx->new_input) {
        ctx->mem[instr.raws[0]] = ctx->input;
        ctx->new_input = 0;
        ctx->status = STATUS_RUN;
    } else {
        ctx->status = STATUS_WAIT;
        ctx->pc -= 1 + OPCODE_PARAM_COUNTS[OP_LD];
    }
}

void out(struct instruction instr, struct context *ctx) {
    ctx->output = instr.values[0];
    ctx->new_output = 1;
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
    ctx->status = STATUS_HALT;
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

void print_array(int *arr, int n, int w) {
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
        printf("%d\t", arr[i]);
    }
    printf("\n");
}

void print_context(struct context *ctx) {
    printf("Memory:\n");
    print_array(ctx->mem, ctx->n, 10);
    printf("\nInput: %d, new: %d\n", ctx->input, ctx->new_input);
    printf("\nOutput: %d, new: %d\n", ctx->output, ctx->new_output);
    printf("\nstatus: %d, n: %d, pc: %d\n", ctx->status, ctx->n, ctx->pc);
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

int tick(struct context *ctx) {
    if (ctx->status != STATUS_HALT && ctx->pc < ctx->n) {
        struct instruction instr = get_instr(ctx);
        OP_FUNCS[instr.opcode](instr, ctx);
    }

    return ctx->status;
}

void run_until_stop(struct context *ctx) {
    while (tick(ctx) == STATUS_RUN);
}

void give(struct context *ctx, int input) {
    if (ctx->new_input == 1)
        printf("warning: overwriting unreceived input\n");
    ctx->input = input;
    ctx->new_input = 1;
}

int take(struct context *ctx) {
    if (ctx->new_output == 0)
        printf("warning: taking already used output\n");
    ctx->new_output = 0;
    return ctx->output;
}

void init_context(struct context *ctx) {
    ctx->status = STATUS_RUN;

    /* load program to initial memory */
    ctx->n = 0;
    while (scanf("%d,", ctx->mem + ctx->n) == 1) {
        ctx->n++;
    }
    ctx->pc = 0;

    ctx->input = 0;
    ctx->new_input = 0;

    ctx->output = 0;
    ctx->new_output = 0;
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
        int *dst = &seqs[*i*len];
        *i += 1;
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

int part1(struct context ctx) {
    int seq[AMP_COUNT];
    for (int i = 0; i < AMP_COUNT; i++)
        seq[i] = i;
    int n;
    int *perms = permutations(seq, AMP_COUNT, &n);

    int max = 0;
    for (int i = 0; i < n; i++) {
        int output = 0;

        for (int j = 0; j < AMP_COUNT; j++) {
            struct context amp = ctx;
            int phase = perms[AMP_COUNT*i+j];

            give(&amp, phase);
            run_until_stop(&amp);

            give(&amp, output);
            run_until_stop(&amp);

            output = take(&amp);
        }

        if (output > max) {
            max = output;
        }
    }

    return max;
}

int part2(struct context ctx) {
    int seq[AMP_COUNT];
    for (int i = 0; i < AMP_COUNT; i++)
        seq[i] = i+AMP_COUNT;
    int n;
    int *perms = permutations(seq, AMP_COUNT, &n);

    int max = 0;
    for (int i = 0; i < n; i++) {
        struct context amps[AMP_COUNT];
        for (int j = 0; j < AMP_COUNT; j++) {
            amps[j] = ctx;
            int phase = perms[i*AMP_COUNT+j];
            give(&amps[j], phase);
            run_until_stop(&amps[j]);
        }

        int all_halted = 0;
        int signals[AMP_COUNT] = {0};
        while (!all_halted) {
            all_halted = 1;
            for (int j = 0; j < AMP_COUNT; j++) {
                give(&amps[j], signals[j]);
                run_until_stop(&amps[j]);
                signals[(j+1)%AMP_COUNT] = take(&amps[j]);

                if (amps[j].status != STATUS_HALT)
                    all_halted = 0;
            }
        }

        if (signals[0] > max) {
            max = signals[0];
        }
    }

    return max;
}

int main(void) {
    initialize();
    struct context ini;
    init_context(&ini);

    printf("part1: %d\n", part1(ini));
    printf("part2: %d\n", part2(ini));

    return EXIT_SUCCESS;
}
