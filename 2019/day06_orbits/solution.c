#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MIN(a,b) (((a)<(b))?(a):(b))

#define MAX_DIRECT 1700
#define MAX_OBJECTS MAX_DIRECT*2
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
    return memcmp(a->name, b->name, KEYLEN);
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

int get_path(struct object *obj, char *name, char *path, int depth) {
    if (strncmp(obj->name, name, KEYLEN) == 0) {
        path[KEYLEN*depth] = '\0';
        return 1;
    } else {
        for (int i = 0; i < obj->n; i++) {
            if (get_path(obj->sats[i], name, path, depth+1)) {
                memcpy(path+KEYLEN*depth, obj->name, KEYLEN);
                return 1;
            }
        }
        return 0;
    }
}

int count_transits(struct object *com, char *name1, char *name2)  {
    char path1[KEYLEN*MAX_OBJECTS];
    get_path(com, name1, path1, 0);
    char path2[KEYLEN*MAX_OBJECTS];
    get_path(com, name2, path2, 0);

    /* distance to each object */
    int d1 = strlen(path1)/KEYLEN;
    int d2 = strlen(path2)/KEYLEN;

    /* distance to common parent */
    int p = 0;
    while (memcmp(path1+p*KEYLEN, path2+p*KEYLEN, KEYLEN) == 0 &&
           p < MIN(d1, d2)) {
        p++;
    }

    /* distance from object1 to parent plus parent to object2 */
    return (d1-p) + (d2-p);
}

int main(void) {
    /* read lines */
    char direct[MAX_DIRECT][LINELEN];
    int n = 0;
    while (scanf("%s\n", direct[n]) > 0) {
        n++;
    }

    /* create objects */
    struct object objs[MAX_OBJECTS] = {0};
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

    int i_com = binary_search(objs, sizeof(struct object), objc, "COM", objcmp);
    struct object *com = &objs[i_com];

    int orbits = count_orbits(com, 0);
    int transits = count_transits(com, "YOU", "SAN");

    printf("part1: %d\n", orbits);
    printf("part2: %d\n", transits);

    return EXIT_SUCCESS;
}
