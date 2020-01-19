#include <stdio.h>

#define REGPRINT(x) printf("%s\na: %d\nb: %d\nc: %d\nd: %d\n" \
                               "e: %d\nf: %d\ng: %d\nh: %d\n\n", \
                           x, a, b, c, d, e, f, g, h);

//#define DEBUG

int main() {
    int a = 0, b = 0,
        c = 0, d = 0,
        e = 0, f = 0,
        g = 0, h = 0;

    int l1 = 84;
    int l8 = -17000;
    int l31 = -17;

    b = l1;
    c = b;

#ifndef DEBUG
    b = 100*b + 100000;
    c = b - l8;
#endif

    while (1) {
        int is_prime = 1;

        d = 2;
        while (d <= b/2) {
            if (b%d == 0 && b/d >= 2) {
                is_prime = 0;
            }
            d++;
        }

        if (!is_prime)
            h++;

        if (b == c) {
            REGPRINT("final");
            return;
        }

        b -= l31;
    }
}
