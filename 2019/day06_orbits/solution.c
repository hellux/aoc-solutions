#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_DIRECT 1700
#define KEYLEN 3
#define LINELEN KEYLEN*2+2

struct object {
    char name[KEYLEN+1];
    struct object **sats;
    int cap;
    int n;
};

void init_obj(struct object *obj, char *name) {
    memcpy(obj->name, name, KEYLEN);
    obj->name[KEYLEN] = '\0';
    obj->cap = 1;
    obj->sats = malloc(obj->cap*sizeof(struct object*));
    obj->n = 0;
}

void obj_add_satellite(struct object *sat, struct object *body) {
    if (body->n == body->cap) {
        body->cap *= 2;
        body->sats = realloc(body->sats, body->cap*sizeof(sat));
    }
    body->sats[body->n++] = sat;
}

int objcmp(struct object *a, struct object *b) {
    return memcmp(a->name, b->name, 3);
}

int binary_search(void *base, size_t nmemb, size_t size, void *key,
                  int (*compar)(const void *, const void *)) {
    int start = 0;
    int end = size;
    while (start < end) {
        int current = (start+end) / 2;
        int c = compar(base+(current*nmemb), key);
        if (c < 0) {
            start = current+1;
        } else if (c > 0) {
            end = current;
        } else {
            return current;
        }
    }

    return -1;
}

int count_orbits(struct object *obj, int depth) {
    int orbits = depth;
    for (int i = 0; i < obj->n; i++) {
        orbits += count_orbits(obj->sats[i], depth + 1);
    }
    return orbits;
}

int main(void) {
    /* read lines */
    char direct[MAX_DIRECT][LINELEN];
    int n = 0;
    while (scanf("%s\n", direct[n]) > 0) {
        n++;
    }

    /* create objects */
    struct object objs[MAX_DIRECT*2] = {0};
    int objc = 0;
    for (int i = 0; i < n; i++) {
        char *body = direct[i];
        char *sat = direct[i]+KEYLEN+1;

        int i_body = binary_search(objs, sizeof(struct object),
                                   objc, body, objcmp);
        int i_sat = binary_search(objs, sizeof(struct object),
                                  objc, sat, objcmp);

        if (i_body < 0) {
            init_obj(&objs[objc++], body);
        }
        if (i_sat < 0) {
            init_obj(&objs[objc++], sat);
        }

        qsort(objs, objc, sizeof(struct object), objcmp);
        /* XXX not so optimal */
    }

    /* link satellites to objects */
    for (int i = 0; i < n; i++) {
        char *body = direct[i];
        char *sat = direct[i]+KEYLEN+1;

        int i_body = binary_search(objs, sizeof(struct object),
                                   objc, body, objcmp);
        int i_sat = binary_search(objs, sizeof(struct object),
                                  objc, sat, objcmp);

        obj_add_satellite(&objs[i_sat], &objs[i_body]);
    }

    int i_com = binary_search(objs, sizeof(struct object),
                              objc, "COM", objcmp);
    int orbits = count_orbits(&objs[i_com], 0);

    printf("part1: %d\n", orbits);

    return EXIT_SUCCESS;
}
