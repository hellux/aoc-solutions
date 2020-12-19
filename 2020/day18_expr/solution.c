#include <stdio.h>
#include <stdlib.h>

struct stream {
    char c[15000];
    int i;
};

typedef int precedence[256];

long eval_stream(struct stream *s, precedence p, int bp) {
    long lhs;
    char c = s->c[s->i++];
    switch (c) {
        case '(':
            lhs = eval_stream(s, p, 0);
            break;
        default:
            lhs = c - '0';
    }

    while (1) {
        char c = s->c[s->i];
        int lbp = p[(int)c];

        if (lbp < bp)
            break;

        s->i++;

        if (c == ')' || c == '\0')
            break;

        long rhs = eval_stream(s, p, lbp + 1);
        switch (c) {
            case '+': lhs += rhs; break;
            case '*': lhs *= rhs; break;
            default: exit(c);
        }
    }

    return lhs;
}

long part1(struct stream s) {
    precedence p = {0};
    p['+'] = 1;
    p['*'] = 1;
    return eval_stream(&s, p, 0);
}

long part2(struct stream s) {
    precedence p = {0};
    p['+'] = 2;
    p['*'] = 1;
    return eval_stream(&s, p, 0);
}

int main() {
    struct stream s = {"(", 0};

    char c;
    int n = 1;
    while ((c = (char)getchar()) && c != EOF) {
        switch (c) {
            case ' ': continue;
            case '\n':
                s.c[n++] = ')';
                s.c[n++] = '+';
                s.c[n++] = '(';
                break;
            default:
                s.c[n++] = c;
        }
    }
    s.c[n-2] = '\0';

    printf("%ld\n", part1(s));
    printf("%ld\n", part2(s));

    return EXIT_SUCCESS;
}
