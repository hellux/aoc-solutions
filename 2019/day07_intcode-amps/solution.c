#include <stdlib.h>
#include <stdio.h>

#include "../intcode.h"

#define AMP_COUNT 5

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
    int *seqs = malloc((*n) * length * sizeof(int));

    permute(seq, 0, length-1, length, seqs, &i);

    return seqs;
}

integer part1(struct context ctx) {
    int seq[AMP_COUNT];
    for (int i = 0; i < AMP_COUNT; i++)
        seq[i] = i;
    int n;
    int *perms = permutations(seq, AMP_COUNT, &n);

    integer max = 0;
    for (int i = 0; i < n; i++) {
        integer output = 0;

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

integer part2(struct context ctx) {
    int seq[AMP_COUNT];
    for (int i = 0; i < AMP_COUNT; i++)
        seq[i] = i+AMP_COUNT;
    int n;
    int *perms = permutations(seq, AMP_COUNT, &n);

    integer max = 0;
    for (int i = 0; i < n; i++) {
        struct context amps[AMP_COUNT];
        for (int j = 0; j < AMP_COUNT; j++) {
            amps[j] = ctx;
            int phase = perms[i*AMP_COUNT+j];
            give(&amps[j], (integer) phase);
            run_until_stop(&amps[j]);
        }

        int all_halted = 0;
        integer signals[AMP_COUNT] = {0};
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
    struct context ini;
    init_context(&ini);

    printf("part1: %ld\n", part1(ini));
    printf("part2: %ld\n", part2(ini));

    return EXIT_SUCCESS;
}
