#include <stdio.h>
#include <stdlib.h>

struct stream {
    char c[80];
    int i;
};

long eval_stream(struct stream *s);

long eval_atom(struct stream *s) {
    char c = s->c[s->i++];
    switch (c) {
        case '(':
            return eval_stream(s);
        default:
            return c - '0';
    }
}

long eval_stream(struct stream *s) {
    long lhs = eval_atom(s);

    while (1) {
        char c = s->c[s->i++];
        if (c == ')' || c == '\0') break;

        long rhs = eval_atom(s);
        switch (c) {
            case '+': lhs += rhs; break;
            case '*': lhs *= rhs; break;
            default: exit(c);
        }
    }

    return lhs;
}

long part1(struct stream *streams, int n) {
    long sum = 0;

    for (int i = 0; i < n; i++) {
        sum += eval_stream(streams+i);
    }

    return sum;
}

int main() {
    struct stream streams[400] = {0};
    int n = 0;

    char c;
    int ic = 0;
    while ((c = (char)getchar()) && c != EOF) {
        switch (c) {
            case ' ': continue;
            case '\n':
                streams[n++].c[ic] = '\0';
                ic = 0;
                break;
            default:
                streams[n].c[ic++] = c;
        }
    }

    printf("%ld\n", part1(streams, n));

    return EXIT_SUCCESS;
}
