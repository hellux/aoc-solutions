#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_INPUT_SIZE 650
#define R 10000
#define MSG_SIZE 8
#define OFFSET_LENGTH 7

typedef void transform(uint8_t *in, uint8_t *out, size_t n);

uint8_t *repeat100(transform t, uint8_t *in, size_t n) {
    uint8_t *a = malloc(n);
    uint8_t *b = malloc(n);
    memcpy(a, in, n);

    for (int i = 0; i < 100; i++) {
        if (i % 2 == 0) {
            t(a, b, n);
        } else {
            t(b, a, n);
        }
    }

    free(b);
    return a;
}

uint8_t *get_signal(size_t *n) {
    char c;
    size_t i = 0;
    uint8_t *signal = malloc(MAX_INPUT_SIZE);
    while (scanf("%c", &c) == 1)
        signal[i++] = (uint8_t) (c-'0');
    *n = i;

    return signal;
}

void print_message(uint8_t *msg) {
    char str[MSG_SIZE];
    for (int i = 0; i < MSG_SIZE; i++) {
        str[i] = (char) (msg[i]+'0');
    }
    printf("%s\n", str);
}

/* specialized fft only correct for second half of signal */
void fft2(uint8_t *in, uint8_t *out, size_t n) {
    out[n-1] = in[n-1];
    for (int row = (int) n-1; row >= 0; row--) {
        out[row] = (uint8_t) (out[row+1] + in[row]) % 10;
    }
}

void fft1(uint8_t *in, uint8_t *out, size_t n) {
    for (size_t row = 0; row < n/2; row++) {
        int sum = 0;
        int mult = 1;
        size_t i = row;

        while (i < n) {
            for (size_t j = 0; j < row+1 && i < n; j++) {
                sum += mult * ((int) in[i++]);
            }
            i += row+1; /* skip zeroes */
            mult = -mult;
        }

        out[row] = (uint8_t) (abs(sum) % 10);
    }

    fft2(in+n/2, out+n/2, n/2+(n%2));
}

void part1(uint8_t *signal, size_t n) {
    uint8_t *transformed = repeat100(fft1, signal, n);
    print_message(transformed);
    free(transformed);
}

void part2(uint8_t *signal, size_t n) {
    /* read offset */
    size_t offset = 0;
    size_t exponent = 1;
    for (int i = OFFSET_LENGTH-1; i >= 0; i--) {
        offset += signal[i] * exponent;
        exponent *= 10;
    }

    if (offset + MSG_SIZE < n*R) {
        size_t nr = n*R-offset;

        /* get signal right of offset */
        uint8_t *right = malloc(nr);
        for (size_t i = 0; i < nr; i++) {
            right[i] = signal[(i + offset) % n];
        }

        /* transform right side with naive fft variant */
        uint8_t *transformed = repeat100(fft2, right, nr);
        print_message(transformed);
        free(transformed);

        free(right);
    } else {
        printf("message outside result: offset=%lu, N=%lu\n", offset, n*R);
    }
}

int main() {
    size_t n = 0;
    uint8_t *signal = get_signal(&n);

    part1(signal, n);
    part2(signal, n);

    free(signal);

    return EXIT_SUCCESS;
}
